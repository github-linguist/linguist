int ethiopian(int n1, int n2) pure nothrow
in {
    assert(n1 >= 0, "Multiplier can't be negative");
} body {
    static enum doubleNum = (in int n) pure nothrow => n * 2;
    static enum halveNum = (in int n) pure nothrow => n / 2;
    static enum isEven = (in int n) pure nothrow => !(n & 1);

    int result;
    while (n1 >= 1) {
        if (!isEven(n1))
            result += n2;
        n1 = halveNum(n1);
        n2 = doubleNum(n2);
    }

    return result;
} unittest {
    assert(ethiopian(77, 54) == 77 * 54);
    assert(ethiopian(8, 923) == 8 * 923);
    assert(ethiopian(64, -4) == 64 * -4);
}

void main() {
    import std.stdio;

    writeln("17 ethiopian 34 is ", ethiopian(17, 34));
}
