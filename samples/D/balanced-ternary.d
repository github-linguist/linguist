import std.stdio, std.bigint, std.range, std.algorithm;

struct BalancedTernary {
    // Represented as a list of 0, 1 or -1s,
    // with least significant digit first.
    enum Dig : byte { N=-1, Z=0, P=+1 } // Digit.
    const Dig[] digits;

    // This could also be a BalancedTernary template argument.
    static immutable string dig2str = "-0+";

    immutable static Dig[dchar] str2dig; // = ['+': Dig.P, ...];
    nothrow static this() {
        str2dig = ['+': Dig.P, '-':  Dig.N, '0': Dig.Z];
    }

    immutable pure nothrow static Dig[2][] table =
        [[Dig.Z, Dig.N], [Dig.P, Dig.N], [Dig.N, Dig.Z],
         [Dig.Z, Dig.Z], [Dig.P, Dig.Z], [Dig.N, Dig.P],
         [Dig.Z, Dig.P]];

    this(in string inp) const pure {
        this.digits = inp.retro.map!(c => str2dig[c]).array;
    }

    this(in long inp) const pure /*nothrow*/ {
        this.digits = _bint2ternary(inp.BigInt);
    }

    this(in BigInt inp) const pure /*nothrow*/ {
        this.digits = _bint2ternary(inp);
    }

    this(in BalancedTernary inp) const pure nothrow {
        // No need to dup, they are virtually immutable.
        this.digits = inp.digits;
    }

    private this(in Dig[] inp) /*inout*/ pure nothrow {
        this.digits = inp;
    }

    static Dig[] _bint2ternary(in BigInt n) pure /*nothrow*/ {
        static py_div(T1, T2)(in T1 a, in T2 b) pure /*nothrow*/ {
            if (a < 0) {
                return (b < 0) ?
                       -a / -b :
                       -(-a / b) - (-a % b != 0 ? 1 : 0);
            } else {
                return (b < 0) ?
                       -(a / -b) - (a % -b != 0 ? 1 : 0) :
                       a / b;
            }
        }

        if (n == 0) return [];
        // This final switch in D v.2.064 is fake, not enforced.
        final switch (((n % 3) + 3) % 3) { // (n % 3) is the remainder.
            case 0: return Dig.Z ~ _bint2ternary(py_div(n, 3));
            case 1: return Dig.P ~ _bint2ternary(py_div(n, 3));
            case 2: return Dig.N ~ _bint2ternary(py_div(n + 1, 3));
        }
    }

    @property BigInt toBint() const pure /*nothrow*/ {
        return reduce!((y, x) => x + 3 * y)(0.BigInt, digits.retro);
    }

    string toString() const pure nothrow {
        if (digits.empty) return "0";
        return digits.retro.map!(d => dig2str[d + 1]).array;
    }

    static const(Dig)[] neg_(in Dig[] digs) pure nothrow {
        return digs.map!(a => -a).array;
    }

    BalancedTernary opUnary(string op:"-")() const pure nothrow {
        return BalancedTernary(neg_(this.digits));
    }

    static const(Dig)[] add_(in Dig[] a, in Dig[] b, in Dig c=Dig.Z)
    pure nothrow {
        const a_or_b = a.length ? a : b;
        if (a.empty || b.empty) {
            if (c == Dig.Z)
                return a_or_b;
            else
                return BalancedTernary.add_([c], a_or_b);
        } else {
            // (const d, c) = table[...];
            const dc = table[3 + (a.length ? a[0] : 0) +
                             (b.length ? b[0] : 0) + c];
            const res = add_(a[1 .. $], b[1 .. $], dc[1]);
            // Trim leading zeros.
            if (res.length || dc[0] != Dig.Z)
                return [dc[0]] ~ res;
            else
                return res;
        }
    }

    BalancedTernary opBinary(string op:"+")(in BalancedTernary b)
    const pure nothrow {
        return BalancedTernary(add_(this.digits, b.digits));
    }

    BalancedTernary opBinary(string op:"-")(in BalancedTernary b)
    const pure nothrow {
        return this + (-b);
    }

    static const(Dig)[] mul_(in Dig[] a, in Dig[] b) pure nothrow {
        if (a.empty || b.empty) {
            return [];
        } else {
            const y = Dig.Z ~ mul_(a[1 .. $], b);
            final switch (a[0]) {
                case Dig.N: return add_(neg_(b), y);
                case Dig.Z: return add_([], y);
                case Dig.P: return add_(b, y);
            }
        }
    }

    BalancedTernary opBinary(string op:"*")(in BalancedTernary b)
    const pure nothrow {
        return BalancedTernary(mul_(this.digits, b.digits));
    }
}

void main() {
    immutable a = BalancedTernary("+-0++0+");
    writeln("a: ", a.toBint, ' ', a);

    immutable b = BalancedTernary(-436);
    writeln("b: ", b.toBint, ' ', b);

    immutable c = BalancedTernary("+-++-");
    writeln("c: ", c.toBint, ' ', c);

    immutable r = a * (b - c);
    writeln("a * (b - c): ", r.toBint, ' ', r);
}
