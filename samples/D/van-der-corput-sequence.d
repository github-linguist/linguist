import std.stdio, std.algorithm, std.range;

double vdc(int n, in double base=2.0) pure nothrow {
    double vdc = 0.0, denom = 1.0;
    while (n) {
        denom *= base;
        vdc += (n % base) / denom;
        n /= base;
    }
    return vdc;
}

void main() {
    foreach (b; 2 .. 6)
        writeln("\nBase ", b, ": ", iota(10).map!(n => vdc(n, b))());
}
