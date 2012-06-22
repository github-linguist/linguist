package Plack::Response;
use strict;
use warnings;
our $VERSION = '0.9988';
$VERSION = eval $VERSION;

use Plack::Util::Accessor qw(body status);
use Carp ();
use Scalar::Util ();
use HTTP::Headers;
use URI::Escape ();

sub code    { shift->status(@_) }
sub content { shift->body(@_)   }

sub new {
    my($class, $rc, $headers, $content) = @_;

    my $self = bless {}, $class;
    $self->status($rc)       if defined $rc;
    $self->headers($headers) if defined $headers;
    $self->body($content)    if defined $content;

    $self;
}

sub headers {
    my $self = shift;

    if (@_) {
        my $headers = shift;
        if (ref $headers eq 'ARRAY') {
            Carp::carp("Odd number of headers") if @$headers % 2 != 0;
            $headers = HTTP::Headers->new(@$headers);
        } elsif (ref $headers eq 'HASH') {
            $headers = HTTP::Headers->new(%$headers);
        }
        return $self->{headers} = $headers;
    } else {
        return $self->{headers} ||= HTTP::Headers->new();
    }
}

sub cookies {
    my $self = shift;
    if (@_) {
        $self->{cookies} = shift;
    } else {
        return $self->{cookies} ||= +{ };
    }
}

sub header { shift->headers->header(@_) } # shortcut

sub content_length {
    shift->headers->content_length(@_);
}

sub content_type {
    shift->headers->content_type(@_);
}

sub content_encoding {
    shift->headers->content_encoding(@_);
}

sub location {
    my $self = shift;
    return $self->headers->header('Location' => @_);
}

sub redirect {
    my $self = shift;

    if (@_) {
        my $url = shift;
        my $status = shift || 302;
        $self->location($url);
        $self->status($status);
    }

    return $self->location;
}

sub finalize {
    my $self = shift;
    Carp::croak "missing status" unless $self->status();

    my $headers = $self->headers->clone;
    $self->_finalize_cookies($headers);

    return [
        $self->status,
        +[
            map {
                my $k = $_;
                map {
                    my $v = $_;
                    $v =~ s/\015\012[\040|\011]+/chr(32)/ge; # replace LWS with a single SP
                    $v =~ s/\015|\012//g; # remove CR and LF since the char is invalid here

                    ( $k => $v )
                } $headers->header($_);

            } $headers->header_field_names
        ],
        $self->_body,
    ];
}

sub _body {
    my $self = shift;
    my $body = $self->body;
       $body = [] unless defined $body;
    if (!ref $body or Scalar::Util::blessed($body) && overload::Method($body, q("")) && !$body->can('getline')) {
        return [ $body ];
    } else {
        return $body;
    }
}

sub _finalize_cookies {
    my($self, $headers) = @_;

    while (my($name, $val) = each %{$self->cookies}) {
        my $cookie = $self->_bake_cookie($name, $val);
        $headers->push_header('Set-Cookie' => $cookie);
    }
}

sub _bake_cookie {
    my($self, $name, $val) = @_;

    return '' unless defined $val;
    $val = { value => $val } unless ref $val eq 'HASH';

    my @cookie = ( URI::Escape::uri_escape($name) . "=" . URI::Escape::uri_escape($val->{value}) );
    push @cookie, "domain=" . $val->{domain}   if $val->{domain};
    push @cookie, "path=" . $val->{path}       if $val->{path};
    push @cookie, "expires=" . $self->_date($val->{expires}) if $val->{expires};
    push @cookie, "secure"                     if $val->{secure};
    push @cookie, "HttpOnly"                   if $val->{httponly};

    return join "; ", @cookie;
}

my @MON  = qw( Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec );
my @WDAY = qw( Sun Mon Tue Wed Thu Fri Sat );

sub _date {
    my($self, $expires) = @_;

    if ($expires =~ /^\d+$/) {
        # all numbers -> epoch date
        # (cookies use '-' as date separator, HTTP uses ' ')
        my($sec, $min, $hour, $mday, $mon, $year, $wday) = gmtime($expires);
        $year += 1900;

        return sprintf("%s, %02d-%s-%04d %02d:%02d:%02d GMT",
                       $WDAY[$wday], $mday, $MON[$mon], $year, $hour, $min, $sec);

    }

    return $expires;
}

1;
__END__

=head1 NAME

Plack::Response - Portable HTTP Response object for PSGI response

=head1 SYNOPSIS

  use Plack::Response;

  sub psgi_handler {
      my $env = shift;

      my $res = Plack::Response->new(200);
      $res->content_type('text/html');
      $res->body("Hello World");

      return $res->finalize;
  }

=head1 DESCRIPTION

Plack::Response allows you a way to create PSGI response array ref through a simple API.

=head1 METHODS

=over 4

=item new

  $res = Plack::Response->new;
  $res = Plack::Response->new($status);
  $res = Plack::Response->new($status, $headers);
  $res = Plack::Response->new($status, $headers, $body);

Creates a new Plack::Response object.

=item status

  $res->status(200);
  $status = $res->status;

Sets and gets HTTP status code. C<code> is an alias.

=item headers

  $headers = $res->headers;
  $res->headers([ 'Content-Type' => 'text/html' ]);
  $res->headers({ 'Content-Type' => 'text/html' });
  $res->headers( HTTP::Headers->new );

Sets and gets HTTP headers of the response. Setter can take either an
array ref, a hash ref or L<HTTP::Headers> object containing a list of
headers.

=item body

  $res->body($body_str);
  $res->body([ "Hello", "World" ]);
  $res->body($io);

Gets and sets HTTP response body. Setter can take either a string, an
array ref, or an IO::Handle-like object. C<content> is an alias.

Note that this method doesn't automatically set I<Content-Length> for
the response. You have to set it manually if you want, with the
C<content_length> method (see below).

=item header

  $res->header('X-Foo' => 'bar');
  my $val = $res->header('X-Foo');

Shortcut for C<< $res->headers->header >>.

=item content_type, content_length, content_encoding

  $res->content_type('text/plain');
  $res->content_length(123);
  $res->content_encoding('gzip');

Shortcut for the equivalent get/set methods in C<< $res->headers >>.

=item redirect

  $res->redirect($url);
  $res->redirect($url, 301);

Sets redirect URL with an optional status code, which defaults to 302.

Note that this method doesn't normalize the given URI string. Users of
this module have to be responsible about properly encoding URI paths
and parameters.

=item location

Gets and sets C<Location> header.

Note that this method doesn't normalize the given URI string in the
setter. See above in C<redirect> for details.

=item cookies

  $res->cookies->{foo} = 123;
  $res->cookies->{foo} = { value => '123' };

Returns a hash reference containing cookies to be set in the
response. The keys of the hash are the cookies' names, and their
corresponding values are a plain string (for C<value> with everything
else defaults) or a hash reference that can contain keys such as
C<value>, C<domain>, C<expires>, C<path>, C<httponly>, C<secure>.

C<expires> can take a string or an integer (as an epoch time) and
B<does not> convert string formats such as C<+3M>.

  $res->cookies->{foo} = {
      value => 'test',
      path  => "/",
      domain => '.example.com',
      expires => time + 24 * 60 * 60,
  };

=item finalize

  $res->finalize;

Returns the status code, headers, and body of this response as a PSGI
response array reference.

=back

=head1 AUTHOR

Tokuhiro Matsuno

Tatsuhiko Miyagawa

=head1 SEE ALSO

L<Plack::Request>

=cut
