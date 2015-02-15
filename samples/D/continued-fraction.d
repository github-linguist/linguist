import std.stdio, std.functional, std.traits;

FP calc(FP, F)(in F fun, in int n) pure nothrow if (isCallable!F) {
    FP temp = 0;

    foreach_reverse (immutable ni; 1 .. n + 1) {
        immutable p = fun(ni);
        temp = p[1] / (FP(p[0]) + temp);
    }
    return fun(0)[0] + temp;
}

int[2] fSqrt2(in int n) pure nothrow {
    return [n > 0 ? 2 : 1,   1];
}

int[2] fNapier(in int n) pure nothrow {
    return [n > 0 ? n : 2,   n > 1 ? (n - 1) : 1];
}

int[2] fPi(in int n) pure nothrow {
    return [n > 0 ? 6 : 3,   (2 * n - 1) ^^ 2];
}

alias print = curry!(writefln, "%.19f");

void main() {
    calc!real(&fSqrt2, 200).print;
    calc!real(&fNapier, 200).print;
    calc!real(&fPi, 200).print;
}
