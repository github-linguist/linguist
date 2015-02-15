void main() {
    import std.stdio, std.file;

    dirEntries(".", "*.*", SpanMode.shallow).writeln;
}
