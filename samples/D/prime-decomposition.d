import std.stdio, std.bigint, std.algorithm, std.traits, std.range;

Unqual!T[] decompose(T)(in T number) pure /*nothrow*/
in {
    assert(number > 1);
} body {
    typeof(return) result;
    Unqual!T n = number;

    for (Unqual!T i = 2; n % i == 0; n /= i)
        result ~= i;
    for (Unqual!T i = 3; n >= i * i; i += 2)
        for (; n % i == 0; n /= i)
            result ~= i;

    if (n != 1)
        result ~= n;
    return result;
}

void main() {
    writefln("%(%s\n%)", iota(2, 10).map!decompose);
    decompose(1023 * 1024).writeln;
    BigInt(2 * 3 * 5 * 7 * 11 * 11 * 13 * 17).decompose.writeln;
    decompose(16860167264933UL.BigInt * 179951).writeln;
    decompose(2.BigInt ^^ 100_000).group.writeln;
}
