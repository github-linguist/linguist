import std.stdio, std.algorithm;

T a(T)(T answer) {
    writefln("  # Called function a(%s) -> %s", answer, answer);
    return answer;
}

T b(T)(T answer) {
    writefln("  # Called function b(%s) -> %s", answer, answer);
    return answer;
}

void main() {
    foreach (immutable x, immutable y;
             [false, true].cartesianProduct([false, true])) {
        writeln("\nCalculating: r1 = a(x) && b(y)");
        immutable r1 = a(x) && b(y);
        writeln("Calculating: r2 = a(x) || b(y)");
        immutable r2 = a(x) || b(y);
    }
}
