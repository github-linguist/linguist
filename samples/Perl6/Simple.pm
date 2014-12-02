# ----------------------
# LWP::Simple for Perl 6
# ----------------------
use v6;
use MIME::Base64;
use URI;

class LWP::Simple:auth<cosimo>:ver<0.085>;

our $VERSION = '0.085';

enum RequestType <GET POST>;

has Str $.default_encoding = 'utf-8';
our $.class_default_encoding = 'utf-8';

# these were intended to be constant but that hit pre-compilation issue
my Buf $crlf = Buf.new(13, 10);
my Buf $http_header_end_marker = Buf.new(13, 10, 13, 10);
my Int constant $default_stream_read_len = 2 * 1024;

method base64encode ($user, $pass) {
    my MIME::Base64 $mime .= new();
    my $encoded = $mime.encode_base64($user ~ ':' ~ $pass);
    return $encoded;
}

method get (Str $url) {
    self.request_shell(RequestType::GET, $url)
}

method post (Str $url, %headers = {}, Any $content?) {
    self.request_shell(RequestType::POST, $url, %headers, $content)
}

method request_shell (RequestType $rt, Str $url, %headers = {}, Any $content?) {

    return unless $url;

    my ($scheme, $hostname, $port, $path, $auth) = self.parse_url($url);

    %headers{'Connection'} = 'close';
    %headers{'User-Agent'} //= "LWP::Simple/$VERSION Perl6/$*PERL<compiler><name>";

    if $auth {
        $hostname = $auth<host>;
        my $user = $auth<user>;
        my $pass = $auth<password>;
        my $base64enc = self.base64encode($user, $pass);
        %headers<Authorization> = "Basic $base64enc";
    }

    %headers<Host> = $hostname;

    if ($rt ~~ RequestType::POST && $content.defined) {
        # Attach Content-Length header
        # as recommended in RFC2616 section 14.3.
        # Note: Empty content is also a content,
        # header value equals to zero is valid.
        %headers{'Content-Length'} = $content.encode.bytes;
    }

    my ($status, $resp_headers, $resp_content) =
        self.make_request($rt, $hostname, $port, $path, %headers, $content);

    given $status {

        when / 30 <[12]> / {
            my %resp_headers = $resp_headers.hash;
            my $new_url = %resp_headers<Location>;
            if ! $new_url {
                die "Redirect $status without a new URL?";
            }

            # Watch out for too many redirects.
            # Need to find a way to store a class member
            #if $redirects++ > 10 {
            #    say "Too many redirects!";
            #    return;
            #}

            return self.request_shell($rt, $new_url, %headers, $content);
        }

        when /200/ {
            # should be fancier about charset decoding application - someday
            if  $resp_headers<Content-Type> &&
                $resp_headers<Content-Type> ~~
                    /   $<media-type>=[<-[/;]>+]
                        [ <[/]> $<media-subtype>=[<-[;]>+] ]? /  &&
                (   $<media-type> eq 'text' ||
                    (   $<media-type> eq 'application' &&
                        $<media-subtype> ~~ /[ ecma | java ]script | json/
                    )
                )
            {
                my $charset = 
                    ($resp_headers<Content-Type> ~~ /charset\=(<-[;]>*)/)[0];
                $charset = $charset ?? $charset.Str !!
                    self ?? $.default_encoding !! $.class_default_encoding;
                return $resp_content.decode($charset);
            }
            else {
                return $resp_content;
            }
            
        }

        # Response failed
        default {
            return;
        }
    }

}

method parse_chunks(Blob $b is rw, IO::Socket::INET $sock) {
    my Int ($line_end_pos, $chunk_len, $chunk_start) = (0) xx 3;
    my Blob $content = Blob.new();

    # smallest valid chunked line is 0CRLFCRLF (ascii or other 8bit like EBCDIC)
    while ($line_end_pos + 5 <= $b.bytes) {
        while ( $line_end_pos +4 <= $b.bytes  &&
                $b.subbuf($line_end_pos, 2) ne $crlf
        ) {
            $line_end_pos++
        }
#       say "got here x0x pos ", $line_end_pos, ' bytes ', $b.bytes, ' start ', $chunk_start, ' some data ', $b.subbuf($chunk_start, $line_end_pos +2 - $chunk_start).decode('ascii');
        if  $line_end_pos +4 <= $b.bytes &&
            $b.subbuf(
                $chunk_start, $line_end_pos + 2 - $chunk_start
            ).decode('ascii') ~~ /^(<.xdigit>+)[";"|"\r\n"]/ 
        {

            # deal with case of chunk_len is 0

            $chunk_len = :16($/[0].Str);
#            say 'got chunk len ', $/[0].Str;

            # test if at end of buf??
            if $chunk_len == 0 {
                # this is a "normal" exit from the routine
                return True, $content;
            }

            # think 1CRLFxCRLF
            if $line_end_pos + $chunk_len + 4 <= $b.bytes {
#                say 'inner chunk';
                $content ~= $b.subbuf($line_end_pos +2, $chunk_len);
                $line_end_pos = $chunk_start = $line_end_pos + $chunk_len +4;
            }
            else {
#                say 'last chunk';
                # remaining chunk part len is chunk_len with CRLF
                # minus the length of the chunk piece at end of buffer
                my $last_chunk_end_len = 
                    $chunk_len +2 - ($b.bytes - $line_end_pos -2);
                $content ~= $b.subbuf($line_end_pos +2);
                if $last_chunk_end_len > 2  {
                    $content ~= $sock.read($last_chunk_end_len -2);
                }
                # clean up CRLF after chunk
                $sock.read(min($last_chunk_end_len, 2));

                # this is a` "normal" exit from the routine
                return False, $content;
            }
        }
        else {
#            say 'extend bytes ', $b.bytes, ' start ', $chunk_start, ' data ', $b.subbuf($chunk_start).decode('ascii');
            # maybe odd case of buffer has just part of header at end
            $b ~= $sock.read(20);
        }
    }

#    say join ' ', $b[0 .. 100];
#    say $b.subbuf(0, 100).decode('utf-8');
    die "Could not parse chunk header";
}

method make_request (
    RequestType $rt, $host, $port as Int, $path, %headers, $content?
) {

    my $headers = self.stringify_headers(%headers);

    my IO::Socket::INET $sock .= new(:$host, :$port);
    my Str $req_str = $rt.Stringy ~ " {$path} HTTP/1.1\r\n"
        ~ $headers
        ~ "\r\n";

    # attach $content if given
    # (string context is forced by concatenation)
    $req_str ~= $content if $content.defined;

    $sock.send($req_str);

    my Blob $resp = $sock.read($default_stream_read_len);

    my ($status, $resp_headers, $resp_content) = self.parse_response($resp);


    if (($resp_headers<Transfer-Encoding> || '') eq 'chunked') {
        my Bool $is_last_chunk;
        my Blob $resp_content_chunk;

        ($is_last_chunk, $resp_content) =
            self.parse_chunks($resp_content, $sock);
        while (not $is_last_chunk) {
            ($is_last_chunk, $resp_content_chunk) =
                self.parse_chunks(
                    my Blob $next_chunk_start = $sock.read(1024),
                    $sock
            );
            $resp_content ~= $resp_content_chunk;
        }
    }
    elsif ( $resp_headers<Content-Length>   &&
            $resp_content.bytes < $resp_headers<Content-Length>
    ) {
        $resp_content ~= $sock.read(
            $resp_headers<Content-Length> - $resp_content.bytes
        );
    }
    else { # a bit hacky for now but should be ok
        while ($resp.bytes > 0) {
            $resp = $sock.read($default_stream_read_len);
            $resp_content ~= $resp;
        }
    }

    $sock.close();

    return ($status, $resp_headers, $resp_content);
}

method parse_response (Blob $resp) {

    my %header;

    my Int $header_end_pos = 0;
    while ( $header_end_pos < $resp.bytes &&
            $http_header_end_marker ne $resp.subbuf($header_end_pos, 4)  ) {
        $header_end_pos++;
    }

    if ($header_end_pos < $resp.bytes) {
        my @header_lines = $resp.subbuf(
            0, $header_end_pos
        ).decode('ascii').split(/\r\n/);
        my Str $status_line = @header_lines.shift;

        for @header_lines {
            my ($name, $value) = .split(': ');
            %header{$name} = $value;
        }
        return $status_line, %header.item, $resp.subbuf($header_end_pos +4).item;
    }

    die "could not parse headers";
#    if %header.exists('Transfer-Encoding') && %header<Transfer-Encoding> ~~ m/:i chunked/ {
#        @content = self.decode_chunked(@content);
#    }

}

method getprint (Str $url) {
    my $out = self.get($url);
    if $out ~~ Buf { $*OUT.write($out) } else { say $out }
}

method getstore (Str $url, Str $filename) {
    return unless defined $url;

    my $content = self.get($url);
    if ! $content {
        return
    }

    my $fh = open($filename, :bin, :w);
    if $content ~~ Buf {
        $fh.write($content)
    }
    else {
        $fh.print($content)
    }

    $fh.close; 
}

method parse_url (Str $url) {
    my URI $u .= new($url);
    my $path = $u.path_query;
    
    my $user_info = $u.grammar.parse_result<URI_reference><URI><hier_part><authority><userinfo>;
    
    return (
        $u.scheme, 
        $user_info ?? "{$user_info}@{$u.host}" !! $u.host, 
        $u.port, 
        $path eq '' ?? '/' !! $path,
        $user_info ?? {
            host => $u.host,
            user => ~ $user_info<likely_userinfo_component>[0],
            password => ~ $user_info<likely_userinfo_component>[1]
        } !! Nil
    );    
}

method stringify_headers (%headers) {
    my Str $str = '';
    for sort %headers.keys {
        $str ~= $_ ~ ': ' ~ %headers{$_} ~ "\r\n";
    }
    return $str;
}

