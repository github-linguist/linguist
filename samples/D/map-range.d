import std.stdio;

double mapRange(in double[] a, in double[] b, in double s)
pure nothrow {
    return  b[0] + ((s - a[0]) * (b[1] - b[0]) / (a[1] - a[0]));
}

void main() {
    const r1 = [0.0, 10.0];
    const r2 = [-1.0, 0.0];
    foreach (s; 0 .. 11)
        writefln("%2d maps to %5.2f", s, mapRange(r1, r2, s));
}
