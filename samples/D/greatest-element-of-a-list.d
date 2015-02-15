void main() {
    import std.stdio, std.algorithm;

    [9, 4, 3, 8, 5].reduce!max.writeln;
}
