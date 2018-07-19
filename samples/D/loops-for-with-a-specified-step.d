import std.stdio, std.range;

void main() {
    // Print odd numbers up to 9.
    for (int i = 1; i < 10; i += 2)
        writeln(i);

    // Alternative way.
    foreach (i; iota(1, 10, 2))
        writeln(i);
}
