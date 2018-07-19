import std.stdio, std.math;

real nthroot(in int n, in real A, in real p=0.001) pure nothrow {
    real[2] x = [A, A / n];
    while (abs(x[1] - x[0]) > p)
        x = [x[1], ((n - 1) * x[1] + A / (x[1] ^^ (n-1))) / n];
    return x[1];
}

void main() {
    writeln(nthroot(10, 7131.5 ^^ 10));
    writeln(nthroot(6, 64));
}
