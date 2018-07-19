import std.math, std.numeric, std.traits, std.conv, std.complex;


struct Quat(T) if (isFloatingPoint!T) {
    alias Complex!T CT;

    union {
        struct { T re, i, j, k; } // Default init to NaN
        struct { CT x, y; }
        struct { T[4] vector; }
    }

    string toString() const /*pure nothrow*/ {
        return text(vector);
    }

    @property T norm2() const pure nothrow { /// Norm squared
        return re ^^ 2 + i ^^ 2 + j ^^ 2 + k ^^ 2;
    }

    @property T abs() const pure nothrow { /// Norm
        return sqrt(norm2);
    }

    @property T arg() const pure nothrow { /// Theta
        return acos(re / abs); // this may be incorrect...
    }

    @property Quat!T conj() const pure nothrow { /// Conjugate
        return Quat!T(re, -i, -j, -k);
    }

    @property Quat!T recip() const pure nothrow {  /// Reciprocal
        return Quat!T(re / norm2, -i / norm2, -j / norm2, -k / norm2);
    }

    @property Quat!T pureim() const pure nothrow { /// Pure imagery
        return Quat!T(0, i, j, k);
    }

    @property Quat!T versor() const pure nothrow { /// Unit versor
        return this / abs;
    }

    /// Unit versor of imagery part
    @property Quat!T iversor() const pure nothrow {
        return pureim / pureim.abs;
    }

    /// Assignment
    Quat!T opAssign(U : T)(Quat!U z) pure nothrow {
        x = z.x;  y = z.y;
        return this;
    }

    Quat!T opAssign(U : T)(Complex!U c) pure nothrow {
        x = c;  y = 0;
        return this;
    }

    Quat!T opAssign(U : T)(U r) pure nothrow if (isNumeric!U) {
        re = r; i = 0; y = 0;
        return this;
    }

    /// Test for equal, not ordered so no opCmp
    bool opEquals(U : T)(Quat!U z) const pure nothrow {
        return re == z.re && i == z.i && j == z.j && k == z.k;
    }

    bool opEquals(U : T)(Complex!U c) const pure nothrow {
        return re == c.re && i == c.im && j == 0 && k == 0;
    }

    bool opEquals(U : T)(U r) const pure nothrow if (isNumeric!U) {
        return re == r && i == 0 && j == 0 && k == 0;
    }

    /// Unary op
    Quat!T opUnary(string op)() const pure nothrow if (op == "+") {
        return this;
    }

    Quat!T opUnary(string op)() const pure nothrow if (op == "-") {
        return Quat!T(-re, -i, -j, -k);
    }

    /// Binary op, Quaternion on left of op
    Quat!(CommonType!(T,U)) opBinary(string op, U)(Quat!U z)
    const pure nothrow {
        alias typeof(return) C;

        static if (op == "+" ) {
            return C(re + z.re, i + z.i, j + z.j, k + z.k);
        } else static if (op == "-") {
            return C(re - z.re, i - z.i, j - z.j, k - z.k);
        } else static if (op == "*") {
            return C(re * z.re - i * z.i  - j * z.j  - k * z.k,
                     re * z.i  + i * z.re + j * z.k  - k * z.j,
                     re * z.j  - i * z.k  + j * z.re + k * z.i,
                     re * z.k  + i * z.j  - j * z.i  + k * z.re);
        } else static if (op == "/") {
            return this * z.recip;
        }
    }

    /// Extend complex to quaternion
    Quat!(CommonType!(T,U)) opBinary(string op, U)(Complex!U c)
    const pure nothrow {
        return opBinary!op(typeof(return)(c.re, c.im, 0, 0));
    }

    /// For scalar
    Quat!(CommonType!(T,U)) opBinary(string op, U)(U r)
    const pure nothrow if (isNumeric!U) {
        alias typeof(return) C;

        static if (op == "+" ) {
            return C(re + r, i, j, k);
        } else static if (op == "-") {
            return C(re - r, i, j, k);
        } else static if (op == "*") {
            return C(re * r, i * r, j * r, k * r);
        } else static if (op == "/") {
            return C(re / r, i / r, j / r, k / r);
        } else static if (op == "^^") {
            return pow(r);
        }
    }

    /// Power function
    Quat!(CommonType!(T,U)) pow(U)(U r)
    const pure nothrow if (isNumeric!U) {
        return (abs^^r) * exp(r * iversor * arg);
    }

    /// Handle binary op if Quaternion on right of op and left is
    /// not quaternion.
    Quat!(CommonType!(T,U)) opBinaryRight(string op, U)(Complex!U c)
    const pure nothrow {
        alias typeof(return) C;
        auto w = C(c.re, c.im, 0, 0);
        return w.opBinary!(op)(this);
    }

    Quat!(CommonType!(T,U)) opBinaryRight(string op, U)(U r)
    const pure nothrow if (isNumeric!U) {
        alias typeof(return) C;

        static if (op == "+" || op == "*") {
            return opBinary!op(r);
        } else static if (op == "-") {
            return C(r - re , -i, -j, -k);
        } else static if (op == "/") {
            auto w = C(re, i, j, k);
            return w.recip * r;
        }
    }
}


HT exp(HT)(HT z) pure nothrow if (is(HT T == Quat!T)) {
    immutable inorm = z.pureim.abs;
    return std.math.exp(z.re) * (cos(inorm) + z.iversor * sin(inorm));
}

HT log(HT)(HT z) pure nothrow if (is(HT T == Quat!T)) {
    return std.math.log(z.abs) + z.iversor * acos(z.re / z.abs);
}


void main() { // Demo code
    import std.stdio;
    alias Quat!real QR;
    immutable real r = 7;

    immutable QR q  = QR(2, 3, 4, 5),
                 q1 = QR(2, 3, 4, 5),
                 q2 = QR(3, 4, 5, 6);

    writeln("1.             q - norm: ", q.abs);
    writeln("2.         q - negative: ", -q);
    writeln("3.        q - conjugate: ", q.conj);
    writeln("4.                r + q: ", r + q);
    writeln("                  q + r: ", q + r);
    writeln("5.              q1 + q2: ", q1 + q2);
    writeln("6.                r * q: ", r * q);
    writeln("                  q * r: ", q * r);
    writeln("7.              q1 * q2: ", q1 * q2);
    writeln("                q2 * q1: ", q2 * q1);
    writeln("8.  q1 * q2 != q2 * Q1 ? ", q1 * q2 != q2 * q1);

    immutable QR i = QR(0, 1, 0, 0),
                 j = QR(0, 0, 1, 0),
                 k = QR(0, 0, 0, 1);
    writeln("9.1               i * i: ", i * i);
    writeln("                  J * j: ", j * j);
    writeln("                  k * k: ", k * k);
    writeln("              i * j * k: ", i * j * k);
    writeln("9.2             q1 / q2: ", q1 / q2);
    writeln("9.3        q1 / q2 * q2: ", q1 / q2 * q2);
    writeln("           q2 * q1 / q2: ", q2 * q1 / q2);
    writeln("9.4         exp(pi * i): ", exp(PI * i));
    writeln("            exp(pi * j): ", exp(PI * j));
    writeln("            exp(pi * k): ", exp(PI * k));
    writeln("                 exp(q): ", exp(q));
    writeln("                 log(q): ", log(q));
    writeln("            exp(log(q)): ", exp(log(q)));
    writeln("            log(exp(q)): ", log(exp(q)));
    immutable s = log(exp(q));
    writeln("9.5 let s = log(exp(q)): ", s);
    writeln("                 exp(s): ", exp(s));
    writeln("                 log(s): ", log(s));
    writeln("            exp(log(s)): ", exp(log(s)));
    writeln("            log(exp(s)): ", log(exp(s)));
}
