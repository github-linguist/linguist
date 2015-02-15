import std.stdio, std.math, std.bigint;

bool isPrime(in int p) pure nothrow {
    if (p < 2 || p % 2 == 0)
        return p == 2;
    foreach (immutable i; 3 .. cast(uint)real(p).sqrt + 1)
        if (p % i == 0)
            return false;
    return true;
}

bool isMersennePrime(in int p) pure /*nothrow*/ {
    if (!p.isPrime)
        return false;
    if (p == 2)
        return true;
    immutable mp = (1.BigInt << p) - 1;
    auto s = 4.BigInt;
    foreach (immutable _; 3 .. p + 1)
        s = (s ^^ 2 - 2) % mp;
    return s == 0;
}

void main() {
    foreach (immutable p; 2 .. 2_300)
        if (p.isMersennePrime) {
            write('M', p, ' ');
            stdout.flush;
        }
}
