import std.stdio, std.math, std.algorithm, std.range;

int nonSquare(in int n) pure nothrow {
    return n + cast(int)(0.5 + real(n).sqrt);
}

void main() {
    iota(1, 23).map!nonSquare.writeln;

    foreach (immutable i; 1 .. 1_000_000) {
        immutable ns = i.nonSquare;
        assert(ns != (cast(int)real(ns).sqrt) ^^ 2);
    }
}
