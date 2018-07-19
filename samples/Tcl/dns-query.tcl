package require udp;  # Query by UDP more widely supported, but requires external package
package require dns

set host "www.kame.net"
set v4 [dns::resolve $host -type A];    # Specifically get IPv4 address
set v6 [dns::resolve $host -type AAAA]; # Specifically get IPv6 address
while {[dns::status $v4] eq "connect" || [dns::status $v6] eq "connect"} {
    update; # Let queries complete
}
puts "primary addresses of $host are:\n\tIPv4» [dns::address $v4]\n\tIPv6» [dns::address $v6]"
