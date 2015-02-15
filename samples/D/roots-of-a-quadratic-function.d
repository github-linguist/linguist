import std.math, std.traits;

CommonType!(T1, T2, T3)[] naiveQR(T1, T2, T3)
                                 (in T1 a, in T2 b, in T3 c)
pure nothrow if (isFloatingPoint!T1) {
    alias ReturnT = typeof(typeof(return).init[0]);
    if (a == 0)
        return [cast(ReturnT)c / b]; // It's a linear function.
    immutable ReturnT det = b ^^ 2 - 4 * a * c;
    if (det < 0)
        return []; // No real number root.
    immutable SD = sqrt(det);
    return [(-b + SD) / 2 * a, (-b - SD) / 2 * a];
}

CommonType!(T1, T2, T3)[] cautiQR(T1, T2, T3)
                                 (in T1 a, in T2 b, in T3 c)
pure nothrow if (isFloatingPoint!T1) {
    alias ReturnT = typeof(typeof(return).init[0]);
    if (a == 0)
        return [cast(ReturnT)c / b]; // It's a linear function.
    immutable ReturnT det = b ^^ 2 - 4 * a * c;
    if (det < 0)
        return []; // No real number root.
    immutable SD = sqrt(det);

    if (b * a < 0) {
        immutable x = (-b + SD) / 2 * a;
        return [x, c / (a * x)];
    } else {
        immutable x = (-b - SD) / 2 * a;
        return [c / (a * x), x];
    }
}

void main() {
    import std.stdio;
    writeln("With 32 bit float type:");
    writefln("   Naive: [%(%g, %)]", naiveQR(1.0f, -10e5f, 1.0f));
    writefln("Cautious: [%(%g, %)]", cautiQR(1.0f, -10e5f, 1.0f));
    writeln("\nWith 64 bit double type:");
    writefln("   Naive: [%(%g, %)]", naiveQR(1.0, -10e5, 1.0));
    writefln("Cautious: [%(%g, %)]", cautiQR(1.0, -10e5, 1.0));
    writeln("\nWith real type:");
    writefln("   Naive: [%(%g, %)]", naiveQR(1.0L, -10e5L, 1.0L));
    writefln("Cautious: [%(%g, %)]", cautiQR(1.0L, -10e5L, 1.0L));
}
