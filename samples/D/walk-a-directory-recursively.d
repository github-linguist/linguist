void main() {
    import std.stdio, std.file;

    // Recursive breadth-first scan (use SpanMode.depth for
    // a depth-first scan):
    dirEntries("", "*.d", SpanMode.breadth).writeln;
}
