import std.stdio, std.bigint, std.functional;

BigInt factorial(in uint n) {
    alias mfact = memoize!factorial;
    return n ? mfact(n - 1) * n : 1.BigInt;
}

auto cats1(in uint n) {
    return factorial(2 * n) / (factorial(n + 1) * n.factorial);
}

BigInt cats2(in uint n) {
    alias mcats2 = memoize!cats2;
    if (n == 0) return 1.BigInt;
    BigInt sum = 0;
    foreach (immutable i; 0 .. n)
        sum += mcats2(i) * mcats2(n - 1 - i);
    return sum;
}

BigInt cats3(in uint n) {
    alias mcats3 = memoize!cats3;
    return n ? (4*n - 2) * mcats3(n - 1) / (n + 1) : 1.BigInt;
}

void main() {
    foreach (immutable i; 0 .. 15)
        writefln("%2d => %s %s %s", i, i.cats1, i.cats2, i.cats3);
}
