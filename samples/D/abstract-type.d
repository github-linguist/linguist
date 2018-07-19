import std.stdio;

class Foo {
    // abstract methods can have an implementation for
    // use in super calls.
    abstract void foo() {
        writeln("Test");
    }
}

interface Bar {
    void bar();

    // Final interface methods are allowed.
    final int spam() { return 1; }
}

class Baz : Foo, Bar { // Super class must come first.
    override void foo() {
        writefln("Meep");
        super.foo();
    }

    void bar() {}
}

void main() {}
