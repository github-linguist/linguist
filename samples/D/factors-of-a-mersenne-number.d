import std.stdio, std.math, std.traits;

ulong mersenneFactor(in ulong p) pure nothrow {
    static bool isPrime(T)(in T n) pure nothrow {
        if (n < 2 || n % 2 == 0)
            return n == 2;
        for (Unqual!T i = 3; i ^^ 2 <= n; i += 2)
            if (n % i == 0)
                return false;
        return true;
    }

    static long modPow(in long cb, in long ce,in long m) pure nothrow {
        long b = cb;
        long result = 1;
        for (long e = ce; e > 0; e >>= 1) {
            if ((e & 1) == 1)
                result = (result * b) % m;
            b = (b ^^ 2) % m;
        }
        return result;
    }

    immutable ulong limit = cast(ulong)real(2 ^^ p - 1).sqrt;
    for (ulong k = 1; (2 * p * k - 1) < limit; k++) {
        immutable ulong q = 2 * p * k + 1;
        if (isPrime(q) && (q % 8 == 1 || q % 8 == 7) &&
            modPow(2, p, q) == 1)
            return q;
    }
    return 0;
}

void main() {
    writefln("Factor of M929: %s", 929.mersenneFactor);
}
