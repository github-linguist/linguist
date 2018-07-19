import std.stdio, std.math, std.typecons;

real agm(real a, real g, in int bitPrecision=60) pure nothrow {
    do {
        //(a, g) = tuple((a + g) / 2.0, sqrt(a * g));
        immutable ag = tuple((a + g) / 2.0, sqrt(a * g));
        a = ag[0];
        g = ag[1];
    } while (feqrel(a, g) < bitPrecision);
    return a;
}

void main() {
    writefln("%0.19f", agm(1, 1 / sqrt(2.0)));
}
