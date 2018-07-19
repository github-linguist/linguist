import std.stdio, std.random, std.algorithm, std.range, std.functional;

bool biased(in int n) /*nothrow*/ {
    return uniform(0.0, 1.0) < (1.0 / n);
}

int unbiased(in int bias) /*nothrow*/ {
    int a;
    while ((a = bias.biased) == bias.biased) {}
    return a;
}

void main() {
    enum M = 500_000;
    foreach (immutable n; 3 .. 7)
        writefln("%d: %2.3f%%  %2.3f%%", n,
                 M.iota.map!(_=> n.biased).sum * 100.0 / M,
                 M.iota.map!(_=> n.unbiased).sum * 100.0 / M);
}
