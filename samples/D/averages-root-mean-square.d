import std.stdio, std.math, std.algorithm, std.range;

real rms(R)(R d) pure {
    return sqrt(d.reduce!((a, b) => a + b * b) / real(d.length));
}

void main() {
    writefln("%.19f", iota(1, 11).rms);
}
