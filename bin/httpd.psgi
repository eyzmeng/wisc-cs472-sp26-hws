#!/usr/bin/env perl

use v5.36;
use utf8;
use open qw(:locale :std);

use Errno qw(:POSIX);
use POSIX ();

use Fcntl qw(:seek);
use FindBin;
use File::Basename;
use File::Temp 0.21;
use URI;
use URI::file;

use Data::Dumper;
use Encode;
use HTTP::Date;
use HTTP::Status qw(status_message :constants);
use HTML::Entities qw(encode_entities);
use Socket qw(:DEFAULT :crlf);

use Plack::App::Directory;
use Plack::MIME;
use MIME::Types;  # for Accept
use Plack::Util;

use Crypt::PRNG qw(random_bytes_b64u);

use constant HTML_UNSAFE => "\000-\037&<>\177";

BEGIN {
    my $env = $ENV{'PLACK_ENV'} // 'development';
    if ($env eq 'development' || $env eq 'test') {
        require Time::HiRes;
        *LOG = sub {
            local $/ = "\012" if $/ ne "\012";
            my ($secs, $usec) = Time::HiRes::gettimeofday();
            my $decimals = sprintf("%06d", $usec);
            my $locale = POSIX::setlocale(POSIX::LC_ALL, "C");
            my $ctime = POSIX::strftime(
                "%a, %d %b %Y %H:%M:%S.$usec",
                localtime($secs)
            );
            chomp (my $output = join q[], @_);
            print STDERR $output, $/;
            POSIX::setlocale(POSIX::LC_ALL, $locale);
        };
    }
    else {
        *LOG = sub () { 1 };
    }
}

sub quote
{
    Data::Dumper->new(\@_)->Useqq(1)->Indent(0)->Terse(1)->Dump;
}

sub file_path ($base, $path)
{
    my $base_uri = URI::file->new($base);
    my $file_uri = URI->new("${base_uri}${path}");
    # URI::file->file may return undef if file
    # path is not be translatable to our file
    # system.  In that case, it does not exist.
    $file_uri->file;
}

sub dir_path ($base, $path)
{
    my $base_uri = URI::file->new($base);
    my $dir_uri = URI->new("${base_uri}${path}");
    $dir_uri->dir;
}

# I try to be consistent with RFC 3986, but...
sub normpath ($path)
{
    my @path = (q[], q[]);  # Start at root "/"
    LOG "PATH ", quote($path);
    while ($path =~ m!/?([^/]+)|/+!gc) {
        LOG "    TOKE ", quote($1), " (pos @{[pos$path]})";
        my $comp = defined $1 ? $1 : q[];
        if ($comp eq q[] || $comp eq ".") {
            push @path, q[] unless $path[$#path] eq q[];
        }
        elsif ($comp eq "..") {
            if ($path[$#path] eq q[]) {
                # Pop only if we're not at root
                splice @path, -2 if @path > 2;
            } else {
                pop @path;
            }
        }
        else {
            pop @path if $path[$#path] eq q[];
            push @path, $comp;
        }
    }
    LOG "PATH ", quote(\@path);
    my $isdir = $path[$#path] =~ /\A[.]{,2}\z/;
    ($isdir, join '/', @path);
}

sub open_for_read #($fh, $file)
{
    my (undef, $file) = @_;
    if (-d $file) {
        $! = EISDIR;
        return;
    }
    my $ok = open $_[0], '+<:raw', $file;
    Plack::Util::set_io_path($_[0], $file) if $ok;
    $ok;
}

sub html_or_not ($env)
{
    my $mime = MIME::Types->new();
    my $want = $env->{"HTTP_ACCEPT"};
    my @have = map { $mime->type($_) } qw(text/html text/plain);
    defined $want or return "$have[0]";
    $mime->httpAcceptBest($want, @have);
}

sub die_errno_1 ($env, $code, $cause)
{
    my $errno = $!;
    # XXX: The only locale we know here is really
    # POSIX/C.  In the future we might try harder...
    use locale;
    POSIX::setlocale(POSIX::LC_ALL, "C");
    $! = $errno;
    my $mesg = "$!: @{[quote($cause)]}";
    die_error($env, $code, $mesg);
}

sub die_error ($env, $code, $mesg)
{
    my $type = html_or_not($env) or return [
        HTTP_NOT_ACCEPTABLE, [
            "Content-Type" => "text/plain",
            "Content-Length" => 0,
        ], [],
    ];
    my $body = encode("UTF-8", do {
        unless ($type eq 'text/html') {
            $mesg . CRLF;
        } else {
            my $mesg_html = encode_entities($mesg, HTML_UNSAFE);
            my $title = "$code @{[status_message($code)]}";
            my $title_html = encode_entities($title, HTML_UNSAFE);
            <<EOM;
<!DOCTYPE html>
<html lang="en"><head><meta charset="UTF-8"><title>$title_html</title></head>
<body><h1>$title_html</h1>
<p>$mesg_html</p>
</body>
</html>
EOM
        }
    });
    [ $code, [
        "Content-Type" => "$type; charset=UTF-8",
        "Content-Length" => length($body),
        "Accept-Ranges" => 'bytes',
    ], [ $body ] ];
}

sub redirect ($env, $code, $location)
{
    # In the event that $location contains pretty
    # Unicode, encode it for HTTP.  (I'm counting
    # on you to do it, lwp....)
    my $url = URI->new($location);
    my $type = html_or_not($env) or return [
        HTTP_NOT_ACCEPTABLE, [
            "Content-Type" => "text/plain",
            "Content-Length" => 0,
        ], [],
    ];
    my $body = encode("UTF-8", do {
        unless ($type eq 'text/html') {
            $location . CRLF;
        } else {
            my $title = "Redirect";
            my $title_html = encode_entities($title, HTML_UNSAFE);
            my $link_html = encode_entities($location, q["] . HTML_UNSAFE);
            <<EOM
<!DOCTYPE html>
<html lang="en"><head><meta charset="UTF-8"><title>$title_html</title></head>
<body><h1>$title_html</h1>
<p>I have told your browser to redirect you to
<a href="$link_html">$link_html</a>.
If your browser, please click the link for me.</p>
</body>
</html>
EOM
        }
    });
    [ $code, [
        "Content-Type" => "$type; charset=UTF-8",
        "Content-Length" => length($body),
        "Accept-Ranges" => 'bytes',
        "Location" => "$url",
    ], [ $body ] ];
}

# I don't want to import File::Spec :P
my $base_dir;
BEGIN {
    my $base_url = URI::file->new(dirname($FindBin::RealBin));
    $base_dir = URI->new("$base_url/public/html")->dir;
    defined $base_dir or die "BUG: URI round-trip failed?!!";
}

sub main ($env) {
    my $path = $env->{"PATH_INFO"};
    my ($isdir, $cleanpath) = normpath($path);

    my ($fh, $type);
    if ($isdir) {
        # try $uri/index.html
        {
            my $cleanpath = "${cleanpath}index.html";
            my $file = file_path($base_dir, $cleanpath);
            unless (defined $file) {
                warn "URI not translatable to $^O: ",
                    quote($cleanpath);
                $! = ENOENT;
                return die_errno_1($env, HTTP_NOT_FOUND, $path);
            }
            if (open_for_read($fh, $file)) {
                return $path ne $cleanpath
                    ? redirect($env, HTTP_FOUND, $cleanpath)
                    : file_response($env, $fh);
            }
        }
        # try $uri/
        my $file = dir_path($base_dir, $cleanpath);
        if (-d $file) {
            if (-r _) {
                require Plack::App::Directory;
                # Directory listing
                return $path ne $cleanpath
                    ? redirect($env, HTTP_FOUND, $cleanpath)
                    : Plack::App::Directory->new(
                        { root => $base_dir }
                    )->call($env);
            } else {
                $! = EACCES;
            }
        } else {
            $! = ENOENT;
        }
        warn "open(@{[quote($cleanpath)]}): $!";
        return die_errno_1($env, HTTP_NOT_FOUND, $path);
    }

    my $file = file_path($base_dir, $cleanpath);
    unless (defined $file) {
        warn "URI not translatable to $^O: ",
            quote($cleanpath);
        $! = ENOENT;
        return die_errno_1($env, HTTP_NOT_FOUND, $path);
    }
    if (open_for_read($fh, $file)) {
        return $path ne $cleanpath
            ? redirect($env, HTTP_FOUND, $cleanpath)
            : file_response($env, $fh);
    }
    warn "open(@{[quote($cleanpath)]}): $!";
    return die_errno_1($env, HTTP_NOT_FOUND, $path);
}

sub file_response ($env, $fh)
{
    my $path = $fh->path();
    my $size = -s $path;
    my $time = (stat _)[9]; # mtime
    my $type = mime_type($path);
    my $meth = $env->{"REQUEST_METHOD"} // 'GET';

    $meth eq 'HEAD' || $meth eq 'GET'
        or return [ HTTP_METHOD_NOT_ALLOWED, [], [] ];

    if (exists $env->{"HTTP_IF_MODIFIED_SINCE"}) {
        my $since = $env->{"HTTP_IF_MODIFIED_SINCE"};
        my $since_tm = str2time($since);
        if (defined $since_tm && $time <= $since_tm) {
            return [ HTTP_NOT_MODIFIED, [], [] ];
        }
    }

    if (exists $env->{"HTTP_RANGE"}) {
        (my $range = $env->{"HTTP_RANGE"})
        # HTTP/1.1 (RFC9110): The only legal range-unit is "bytes".
            =~ s/^bytes=// or return die_error($env, HTTP_BAD_REQUEST,
            "Range header does not start with bytes=");
        # range-set is a comma-separated list.  Comma
        # may be mixed with OWS (optional whitespace,
        # which is essentially 1*WSP from RFC822).
        my @range = grep { length } split /[ \t]*,[ \t]*/, $range
            or return die_error($env, HTTP_BAD_REQUEST, "No ranges?");
        my (@off, @len);
RNG:    foreach (@range) {
            my ($fst, $lst);
            if (/^([0-9]+)-([0-9]+)$/) {
                $1 <= $2 or return die_error($env,
                    HTTP_BAD_REQUEST, "Invalid int-range: $1-$2");
                $1 < $size or next RNG;  # Not satisfiable
                ($fst, $lst) = ($1, $size - 1 < $2 ? $size - 1 : $2);
            }
            elsif (/^([0-9]+)-$/) {
                $1 < $size or next RNG;  # Not satisfiable
                ($fst, $lst) = ($1, $size - 1);
            }
            # Suffix request
            elsif (/^-([0-9]+)$/) {
                ($fst, $lst) = ($size < $1 ? 0 : $size - $1, $size - 1);
            }
            else {
                return die_error($env,
                    HTTP_BAD_REQUEST, "Invalid range-spec: $_");
            }
            push @off, $fst;
            push @len, $lst - $fst + 1;
        }
        LOG "parse range: off=", quote(\@off), " len=", quote(\@len);
        if (@off == 0) {
            return [ HTTP_RANGE_NOT_SATISFIABLE, [
                "Content-Type" => $type,
                "Content-Range" => "bytes */$size",
            ], [] ];
        }
        if (@off == 1) {
            # PSGI will try to sneak in Content-Length... :(
            # But the Content-Length can trick some HTTP clients
            # (e.g. curl -X HEAD instead of curl -I) into reading
            # that many bytes following the payload, only to be
            # disappointed by how many bytes we actually return.
            my @preamble = ( HTTP_PARTIAL_CONTENT, [
                "Content-Type" => $type,
                "Content-Length" => $len[0],
                "Content-Range" => "bytes $len[0]/$size",
            ] );
            if ($meth eq 'HEAD') {
                return [ @preamble, [] ];
            }
            my $tempfh = File::Temp->new();
            copy_file($tempfh, $fh, $off[0], $len[0]);
            seek $tempfh, 0, SEEK_SET;
            return [ @preamble, $tempfh ];
        }
        # Multiple ranges
        my $boundary = get_safe_boundary($fh, \@off, \@len);
        my @preamble = ( HTTP_PARTIAL_CONTENT, [
            "Content-Type" => "multipart/byteranges; boundary=$boundary",
        ] );
        if ($meth eq 'HEAD') {
            return [ @preamble, [] ];
        }

        my $span = length($boundary);
        my $tempfh = File::Temp->new();

        foreach my $i (0 .. $#off) {
            # XXX: Do we need to START with a CRLF here?
            # The CRLF above us will be the header-body
            # separator, which has always been sufficient
            # for me in e-mail messages....
            print $tempfh CRLF if $i;
            print $tempfh "--$boundary" . CRLF;
            print $tempfh "Content-Type: $type", CRLF;
            my $fst = $off[$i];
            my $lst = $off[$i] + $len[$i] - 1;
            print $tempfh "Content-Range: $fst-$lst/$size", CRLF;
            print $tempfh CRLF;
            copy_file($tempfh, $fh, $off[$i], $len[$i]);
        }
        $tempfh->flush;
        seek $tempfh, 0, SEEK_SET;
        my $tmpsiz = -s $tempfh->filename();
        push @{$preamble[1]}, ("Content-Length" => $tmpsiz);
        return [ @preamble, $tempfh ];
    }

    my @preamble = ( HTTP_OK, [
        "Content-Type" => $type,
        "Content-Length" => $size,
        "Last-Modified" => time2str($time),
        "Accept-Ranges" => 'bytes',
    ] );
    if ($meth eq 'HEAD') {
        return [ @preamble, [] ];
    }
    return [ @preamble, $fh ];
}

# In imitation of Plack::App::File...
sub mime_type ($path)
{
    my $type;
    if (basename($path) eq 'Makefile') {
        $type = 'text/plain';
    }
    defined $type or $type = Plack::MIME->mime_type(basename($path))
        || 'application/octet-stream';
    $type .= '; charset=UTF-8' if $type =~ /^text\//;
    return $type;
}

# Routine that I really don't want to write.
# (I'm a paranoid math kid, sorry. :)
# https://stackoverflow.com/a/2071407/19411800
sub get_safe_boundary ($fh, $off, $len)
{
    my $boundary;
    # 70/4*3 = 52 -- i'm using up all the entropy
    # within the confines of MIME II (RFC 2046)!! :^)
BD: {
        $boundary = random_bytes_b64u(52);
        my $danger = CRLF . $boundary . CRLF;
        my $span = length($danger);
        my $bufsiz = 8192;
        foreach my $i (0 .. $#$off) {
            sysseek($fh, $off->[$i], SEEK_SET) or die "seek error: $!\n";
            my $last = '';
            my $proc = 0;
            my ($nr, $buf);
    RR:     while ((my $need = $len->[$i] - $proc) > 0) {
                $need = $bufsiz if $bufsiz < $need;
                $nr = sysread($fh, $buf, $need) or last RR;
                $proc += $nr;
                if (index($last . $buf, $danger) != -1) {
                    redo BD;
                }
            } continue {
                # The closest possible matching substring
                # intersects with our current buffer at
                # all points but one; so carrying over
                # ($span - 1) bytes should be just enough.
                if ($nr > $span - 1) {
                    $last = substr($buf, $nr - $span + 1);
                } else {
                    $last = $buf;
                }
            }
            if ($proc < $len->[$i]) {
                die "off=$off->[$i] len=$len->[$i] read error? $!\n";
            }
        }
    }
    return $boundary;
}

sub copy_file ($into, $from, $start, $length)
{
    # Use PerlIO to be consistent with print $fh...
    local ($/, $_);
    my $rem = $length;
    seek($from, $start, SEEK_SET) or die "seek error: $!";
    while ($rem > 0) {
        $/ = \($rem > 8192 ? 8192 : $rem);
        defined ($_ = readline($from)) or last;
        print $into $_;
        $rem -= length;
    }
    if ($rem) {
        die "copy error ($rem of $length left): $!";
    }
}

sub NoNo_NastyPath ($app)
{
    sub ($env) {
        my $path = $env->{"PATH_INFO"};
        if ($path =~ m@
        (
        ^$
        | ^ (.*/)? [.]env.* $
        | ^ (.*/)? [^/]+ [-./] config $
        | ^ (.*/)? config ([-./] production)? [.] (json | php | xml | yaml | yml) $
        | ^ (.*/)? docker (-compose) [.] (yaml | yml)
        | ^ /wp
        (?:
          | -1 # someone somehow thought /wp-1/ exists
          | -admin | -aespa | -apxupx | -cli | -close | -config | -db | -dsa | -fox
          | -elp | -error | -find | -mar | -max | -mna | -mce-help | -page-update
          | -pdmx | -plugging | -readlink | -search | -ss | -thesex | -ver | -xxx
        ) (| [.]php | /.*) $
        | ^ .* [.] (db | sql | sqlite) $
        | ^ (.*/)? [.] (aws | config | kube | git | ssh | svn | vscode ) (/.*)? $
        | ^ /etc/shadow $
        | ^ /etc/ssl (| /.*) $
        | ^ (.*/)? (_vti_cnf | _vti_pvt | _vti_script | _vti_txt) (/.*)? $
        | ^ (.*/)? ([.]DS_Store | Thumbs.db | [.]_.+ ) $
        )
        @snix) {
            goto ABYSS;
        }

        if ($path =~ m@^(.*/)? (s?bin|bin|scripts?|utils?) (/.*)?@snix) {
            goto ABYSS;
        }

        return $app->($env);

ABYSS:
        warn " >>> !!! NoNo BANNED @{[quote($path)]} !!! <<<\n";
        return [ HTTP_NOT_FOUND, [], [] ];
    };
}

use Plack::Builder;
my $app = builder {
    enable "Head";
    enable \&NoNo_NastyPath;
    \&main;
};

unless (caller) {
    require Getopt::Long;
    Getopt::Long::Configure(qw(gnu_compat pass_through));
    Getopt::Long::GetOptions(
        # -b is what I'm used to for python's http.server :,)
        'b|o|host=s' => \my $host,
    );
    # For some reason :DEFAULT does not export inet_ntop? :^(
    $host //= Socket::inet_ntop(AF_INET, INADDR_LOOPBACK);
    unshift @ARGV, "--host", $host;
    require Plack::Runner;
    my $plackup;
    $plackup = Plack::Runner->new;
    $plackup->parse_options(@ARGV);
    $plackup->run($app);
} else {
    return $app;
}

# set vi: ai et sts=4 sw=4:
