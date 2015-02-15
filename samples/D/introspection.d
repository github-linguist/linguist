// Some module-level variables (D doesn't have a global scope).
immutable x = 3, y = 100, z = 3_000;
short w = 1; // Not an int, must be ignored.
immutable s = "some string"; // Not an int, must be ignored.

void main() {
    import std.compiler, std.math, std.traits;

    // Compile-time constants of the compiler version:
    static assert(version_major > 1 && version_minor > 50,
                  "I can't cope with this compiler version.");

    immutable bloop = 10;

    // To check if something compiles:
    static if (__traits(compiles, bloop.abs)) {
        pragma(msg, "The expression is compilable.");
        auto x = bloop.abs;
    } else {
        pragma(msg, "The expression can't be compiled.");
    }

    import std.stdio;
    immutable s = 10_000; // Not at module scope, must be ignored.

    int tot = 0;
    /*static*/ foreach (name; __traits(allMembers, mixin(__MODULE__)))
        static if (is(int == Unqual!(typeof(mixin("." ~ name)))))
            tot += mixin("." ~ name);
    writeln("Total of the module-level ints (could overflow): ", tot);
}
