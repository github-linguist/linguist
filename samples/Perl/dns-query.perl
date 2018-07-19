require 5.014;    # Older versions can't resolve IPv6 with just core Socket module

use Socket qw(getaddrinfo getnameinfo);
my ($err, @res) = getaddrinfo("www.kame.net", 0,
                { protocol=>Socket::IPPROTO_TCP } );
die "getaddrinfo error: $err" if $err;

print getnameinfo($_->{addr}, Socket::NI_NUMERICHOST), "\n" for @res
