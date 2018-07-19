import std.stdio, std.math, std.range, std.algorithm;

auto nthRoots(in int n) pure nothrow {
    return n.iota.map!(k => expi(PI * 2 * (k + 1) / n));
}

void main() {
    foreach (immutable i; 1 .. 6)
        writefln("#%d: [%(%5.2f, %)]", i, i.nthRoots);
}
