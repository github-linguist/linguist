import std.stdio: write, writeln;

void main() {
    for (int i; i < 5; i++) {
        for (int j; j <= i; j++)
            write("*");
        writeln();
    }
    writeln();

    foreach (i; 0 .. 5) {
        foreach (j; 0 .. i + 1)
            write("*");
        writeln();
    }
}
