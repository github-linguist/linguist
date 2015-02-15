import std.stdio, std.bigint;

void main() {
    foreach (i; -5 .. 6)
        writeln(i, " ", i & 1, " ", i % 2, " ", BigInt(i) % 2);
}
