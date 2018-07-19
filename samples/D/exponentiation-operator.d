import std.stdio, std.conv;

struct Number(T) {
    T x; // Base.
    alias x this;
    string toString() const { return text(x); }

    Number opBinary(string op)(in int exponent)
    const pure nothrow if (op == "^^") in {
        if (exponent < 0)
            assert (x != 0, "Division by zero");
    } body {
        debug puts("opBinary ^^");

        int zerodir;
        T factor;
        if (exponent < 0) {
            zerodir = +1;
            factor = (cast(T)1) / x;
        } else {
            zerodir = -1;
            factor = x;
        }

        T result = 1;
        int e = exponent;
        while (e != 0)
            if (e % 2 != 0) {
                result *= factor;
                e += zerodir;
            } else {
                factor *= factor;
                e /= 2;
            }

        return Number(result);
    }
}

void main() {
    alias Double = Number!double;
    writeln(Double(2.5) ^^ 5);

    alias Int = Number!int;
    writeln(Int(3) ^^ 3);
    writeln(Int(0) ^^ -2); // Division by zero.
}
