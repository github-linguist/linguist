import std.stdio, std.traits, std.algorithm, std.range;

auto Y(S, T...)(S delegate(T) delegate(S delegate(T)) f) {
    static struct F {
        S delegate(T) delegate(F) f;
        alias f this;
    }
    return (x => x(x))(F(x => f((T v) => x(x)(v))));
}

void main() { // Demo code:
    auto factorial = Y((int delegate(int) self) =>
        (int n) => 0 == n ? 1 : n * self(n - 1)
    );

    auto ackermann = Y((ulong delegate(ulong, ulong) self) =>
        (ulong m, ulong n) {
            if (m == 0) return n + 1;
            if (n == 0) return self(m - 1, 1);
            return self(m - 1, self(m, n - 1));
    });

    writeln("factorial: ", 10.iota.map!factorial);
    writeln("ackermann(3, 5): ", ackermann(3, 5));
}
