T modInverse(T)(T a, T b) pure nothrow {
    if (b == 1)
        return 1;
    T b0 = b,
      x0 = 0,
      x1 = 1;

    while (a > 1) {
        immutable q = a / b;
        auto t = b;
        b = a % b;
        a = t;
        t = x0;
        x0 = x1 - q * x0;
        x1 = t;
    }
    return (x1 < 0) ? (x1 + b0) : x1;
}

void main() {
    import std.stdio;
    writeln(modInverse(42, 2017));
}
