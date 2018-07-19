import std.stdio, std.math, std.conv, std.algorithm, std.array;

double f(in double x) pure nothrow {
    return x.abs.sqrt + 5 * x ^^ 3;
}

void main() {
    double[] data;

    while (true) {
        "Please enter eleven numbers on a line: ".write;
        data = readln.split.map!(to!double).array;
        if (data.length == 11)
            break;
        writeln("Those aren't eleven numbers.");
    }
    foreach_reverse (immutable x; data) {
        immutable y = x.f;
        writefln("f(%0.3f) = %s", x, y > 400 ? "Too large" : y.text);
    }
}
