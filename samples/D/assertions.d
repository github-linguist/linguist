import std.exception: enforce;

int foo(in bool condition) pure nothrow
in {
    // Assertions are used in contract programming.
    assert(condition);
} out(result) {
    assert(result > 0);
} body {
    if (condition)
        return 42;

    // assert(false) is never stripped from the code, it generates an
    // error in debug builds, and it becomes a HALT instruction in
    // -release mode.
    //
    // It's used as a mark by the D type system. If you remove this
    // line the compiles gives an error:
    //
    // Error: function assertions.foo no return exp;
    //   or assert(0); at end of function
    assert(false, "This can't happen.");
}

void main() pure {
    int x = foo(true);

    // A regular assertion, it throws an error.
    // Use -release to disable it.
    // It can be used in nothrow functions.
    assert(x == 42, "x is not 42");

    // This throws an exception and it can't be disabled.
    // There are some different versions of this lazy function.
    enforce(x == 42, "x is not 42");
}
