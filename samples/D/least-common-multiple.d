import std.stdio, std.bigint, std.math;

T gcd(T)(T a, T b) pure /*nothrow*/ {
    while (b) {
        immutable t = b;
        b = a % b;
        a = t;
    }
    return a;
}

T lcm(T)(T m, T n) pure /*nothrow*/ {
    if (m == 0) return m;
    if (n == 0) return n;
    return abs((m * n) / gcd(m, n));
}

void main() {
    lcm(12, 18).writeln;
    lcm("2562047788015215500854906332309589561".BigInt,
        "6795454494268282920431565661684282819".BigInt).writeln;
}
