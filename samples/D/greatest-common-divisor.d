import std.stdio, std.numeric;

long myGCD(in long x, in long y) pure nothrow {
    if (y == 0)
        return x;
    return myGCD(y, x % y);
}

void main() {
    writeln(gcd(15, 10)); // from Phobos
    writeln(myGCD(15, 10));
}
