import std.stdio;

void main() {
    foreach (i; 1 .. 11) {
        write(i);
        if (i % 5 == 0) {
            writeln();
            continue;
        }
        write(", ");
    }
}
