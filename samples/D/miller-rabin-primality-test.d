import std.random;

bool isProbablePrime(in ulong n, in int k) {
    static long modPow(long b, long e, in long m)
    pure nothrow {
        long result = 1;
        while (e > 0) {
            if ((e & 1) == 1) {
                result = (result * b) % m;
            }
            b = (b * b) % m;
            e >>= 1;
        }
        return result;
    }

    if (n < 2 || n % 2 == 0)
        return n == 2;

    ulong d = n - 1;
    ulong s = 0;
    while (d % 2 == 0) {
        d /= 2;
        s++;
    }
    assert(2 ^^ s * d == n - 1);

    outer:
    foreach (_; 0 .. k) {
        ulong a = uniform(2, n);
        ulong x = modPow(a, d, n);
        if (x == 1 || x == n - 1)
            continue;
        foreach (__; 1 .. s) {
            x = modPow(x, 2, n);
            if (x == 1) return false;
            if (x == n - 1) continue outer;
        }
        return false;
    }

    return true;
}

void main() { // demo code
    import std.stdio, std.range, std.algorithm;
    writeln(filter!(n => isProbablePrime(n, 10))(iota(2, 30)));
}
