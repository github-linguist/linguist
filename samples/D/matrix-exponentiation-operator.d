import std.stdio, std.string, std.math, std.array;

struct SquareMat(T = creal) {
    public static string fmt = "%8.3f";
    private alias TM = T[][];
    private TM a;

    public this(in size_t side) pure nothrow
    in {
        assert(side > 0);
    } body {
        a = new TM(side, side);
    }

    public this(in TM m) pure nothrow
    in {
        assert(!m.empty);
        foreach (const row; m)
            assert(m.length == m[0].length);
    } body {
        a.length = m.length;
        foreach (immutable i, const row; m)
            //a[i] = row.dup; // Not nothrow.
            a[i] = row ~ []; // Slower.
    }

    string toString() const {
        return format("<%(%(" ~ fmt ~ ", %)\n %)>", a);
    }

    public static SquareMat identity(in size_t side) pure nothrow {
        SquareMat m;
        m.a.length = side;
        foreach (immutable r, ref row; m.a) {
            row.length = side;
            foreach (immutable c; 0 .. side)
                row[c] = cast(T)(r == c ? 1 : 0);
        }
        return m;
    }

    public SquareMat opBinary(string op:"*")(in SquareMat other)
    const pure nothrow in {
        assert (a.length == other.a.length);
    } body {
        immutable size_t side = other.a.length;
        SquareMat d;
        d.a = new TM(side, side);
        foreach (immutable r; 0 .. side)
            foreach (immutable c; 0 .. side) {
                d.a[r][c] = cast(T)0;
                foreach (immutable k, immutable ark; a[r])
                    d.a[r][c] += ark * other.a[k][c];
            }
        return d;
    }

    // This is the task part ---------------
    public SquareMat opBinary(string op:"^^")(int n) const pure nothrow
    in {
        assert(n >= 0, "Negative exponent not implemented.");
    } body {
        auto sq = SquareMat(this.a);
        auto d = SquareMat.identity(a.length);
        for (; n > 0; sq = sq * sq, n >>= 1)
            if (n & 1)
                d = d * sq;
        return d;
    }
}

void main() {
    alias M = SquareMat!();
    immutable real q = sqrt(0.5);
    immutable m = M([[   q + 0*1.0Li,    q + 0*1.0Li, 0.0L + 0.0Li],
                     [0.0L - q*1.0Li, 0.0L + q*1.0Li, 0.0L + 0.0Li],
                     [0.0L +   0.0Li, 0.0L +   0.0Li, 0.0L + 1.0Li]]);
    M.fmt = "%5.2f";
    foreach (immutable p; [0, 1, 23, 24])
        writefln("m ^^ %d =\n%s", p, m ^^ p);
}
