void main() {
    import std.stdio, std.string, std.algorithm, std.range, std.conv;

    const perms = "ABCD CABD ACDB DACB BCDA ACBD ADCB CDAB DABC
                   BCAD CADB CDBA CBAD ABDC ADBC BDCA DCBA BACD
                   BADC BDAC CBDA DBCA DCAB".split;

    // Version 1: test all permutations.
    const permsSet = perms.zip(0.repeat).assocArray;
    auto perm = cast(ubyte[])perms[0].dup;
    do {
        if (cast(char[])perm !in permsSet)
            writeln(cast(char[])perm);
    } while (perm.nextPermutation);

    // Version 2: XOR all the ASCII values, the uneven one gets
    // flushed out. Based on Perl 6 (via Go).
    enum int len = 4;
    char[len] b = 0;
    foreach (immutable p; perms)
        b[] ^= p[];
    b.writeln;

    // Version 3 : Sum ASCII values.
    immutable rowSum = perms[0].sum;
    len
    .iota
    .map!(i => to!char(rowSum - perms.transversal(i).sum % rowSum))
    .writeln;

    // Version 4: some sort of checksum, Java translation.
    // maxCode will be 36.
    immutable maxCode = reduce!q{a * b}(len - 1, iota(3, len + 1));

    foreach (immutable i; 0 .. len) {
        immutable code = perms.map!(p => perms[0].countUntil(p[i])).sum;

        // Code will come up 3, 1, 0, 2 short of 36.
        perms[0][maxCode - code].write;
    }
}
