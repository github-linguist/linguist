import std.stdio;

struct Trit {
    private enum Val : byte { F = -1, M, T }
    private Val t;
    alias t this;
    static immutable Trit[3] vals = [{Val.F}, {Val.M}, {Val.T}];
    static immutable F = Trit(Val.F); // Not necessary but handy.
    static immutable M = Trit(Val.M);
    static immutable T = Trit(Val.T);

    string toString() const pure nothrow {
        return "F?T"[t + 1  .. t + 2];
    }

    Trit opUnary(string op)() const pure nothrow
    if (op == "~") {
        return Trit(-t);
    }

    Trit opBinary(string op)(in Trit b) const pure nothrow
    if (op == "&") {
        return t < b ? this : b;
    }

    Trit opBinary(string op)(in Trit b) const pure nothrow
    if (op == "|") {
        return t > b ? this : b;
    }

    Trit opBinary(string op)(in Trit b) const pure nothrow
    if (op == "^") {
        return ~(this == b);
    }

    Trit opEquals(in Trit b) const pure nothrow {
        return Trit(cast(Val)(t * b));
    }

    Trit imply(in Trit b) const pure nothrow {
        return -t > b ? ~this : b;
    }
}

void showOperation(string op)(in string opName) {
    writef("\n[%s]\n    F ? T\n  -------", opName);
    foreach (immutable a; Trit.vals) {
        writef("\n%s |", a);
        foreach (immutable b; Trit.vals)
            static if (op == "==>")
                writef(" %s", a.imply(b));
            else
                writef(" %s", mixin("a " ~ op ~ " b"));
    }
    writeln();
}

void main() {
    writeln("[Not]");
    foreach (const a; Trit.vals)
        writefln("%s | %s", a, ~a);

    showOperation!"&"("And");
    showOperation!"|"("Or");
    showOperation!"^"("Xor");
    showOperation!"=="("Equiv");
    showOperation!"==>"("Imply");
}
