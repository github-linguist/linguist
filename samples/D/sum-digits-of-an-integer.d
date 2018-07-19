import std.stdio, std.bigint;

uint sumDigits(T)(T n, in uint base=10) pure /*nothrow*/
in {
    assert(base > 1);
} body {
    typeof(return) total = 0;
    for ( ; n; n /= base)
        total += n % base;
    return total;
}

void main() {
    1.sumDigits.writeln;
    1_234.sumDigits.writeln;
    sumDigits(0xfe, 16).writeln;
    sumDigits(0xf0e, 16).writeln;
    1_234.BigInt.sumDigits.writeln;
}
