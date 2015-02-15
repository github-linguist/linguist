void main() {
    import std.stdio, std.array, std.range, std.algorithm;

    enum n = 12;
    writefln("    %(%4d%)\n%s", iota(1, n+1), "-".replicate(4*n + 4));
    foreach (immutable y; 1 .. n + 1)
        writefln("%4d" ~ " ".replicate(4 * (y - 1)) ~ "%(%4d%)", y,
                 iota(y, n + 1).map!(x => x * y));
}
