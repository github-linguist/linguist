import std.stdio, std.math, std.string, std.conv, std.algorithm,
       std.range;

bool isPrime(in int n) pure nothrow {
    if (n <= 1)
        return false;
    foreach (immutable i; 2 .. cast(int)sqrt(real(n)) + 1)
        if (!(n % i))
            return false;
    return true;
}

bool isTruncatablePrime(bool left)(in int n) pure {
    immutable s = n.text;
    if (s.canFind('0'))
        return false;
    foreach (immutable i; 0 .. s.length)
        static if (left) {
            if (!s[i .. $].to!int.isPrime)
                return false;
        } else {
            if (!s[0 .. i + 1].to!int.isPrime)
                return false;
        }
    return true;
}

void main() {
    enum n = 1_000_000;
    writeln("Largest left-truncatable prime in 2 .. ", n, ": ",
            iota(n, 1, -1).filter!(isTruncatablePrime!true).front);
    writeln("Largest right-truncatable prime in 2 .. ", n, ": ",
            iota(n, 1, -1).filter!(isTruncatablePrime!false).front);
}
