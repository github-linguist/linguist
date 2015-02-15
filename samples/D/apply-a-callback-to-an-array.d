import std.stdio, std.algorithm;

void main() {
    auto items = [1, 2, 3, 4, 5];
    auto m = items.map!(x => x + 5)();
    writeln(m);
}
