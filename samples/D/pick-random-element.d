import std.stdio, std.random;

void main() {
    const items = ["foo", "bar", "baz"];
    items[uniform(0, $)].writeln;
}
