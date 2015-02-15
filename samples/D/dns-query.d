import std.stdio, std.socket;

void main() {
    auto domain = "www.kame.net", port = "80";

    auto a = getAddressInfo(domain, port);
    writefln("IPv4 address for %s: %s", domain, a[0].address);

    a = getAddressInfo(domain, port, AddressFamily.INET6);
    writefln("IPv6 address for %s: %s", domain, a[0].address);
}
