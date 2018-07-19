class U0 : Exception {
    this() @safe pure nothrow { super("U0 error message"); }
}

class U1 : Exception {
    this() @safe pure nothrow { super("U1 error message"); }
}

void foo() {
    import std.stdio;

    foreach (immutable i; 0 .. 2) {
        try {
            i.bar;
        } catch (U0) {
            "Function foo caught exception U0".writeln;
        }
    }
}

void bar(in int i) @safe pure {
    i.baz;
}

void baz(in int i) @safe pure {
    throw i ? new U1 : new U0;
}

void main() {
    foo;
}
