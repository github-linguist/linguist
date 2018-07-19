void main() {
    import std.stdio, std.conv, std.string;

    int a = 10, b = 20;
    try {
        a = readln.strip.to!int;
        b = readln.strip.to!int;
    } catch (StdioException) {}

    if (a < b)
        writeln(a, " is less than ", b);

    if (a == b)
        writeln(a, " is equal to ", b);

    if (a > b)
        writeln(a, " is greater than ", b);
}
