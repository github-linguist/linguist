import std.stdio, std.uri;

void main() {
    writeln(decodeComponent("http%3A%2F%2Ffoo%20bar%2F"));
}
