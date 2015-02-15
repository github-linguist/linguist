import std.stdio, std.random, std.math, std.complex;

void main() {
    char[31][31] table = ' ';

    foreach (immutable _; 0 .. 100) {
        int x, y;
        do {
            x = uniform(-15, 16);
            y = uniform(-15, 16);
        } while(abs(12.5 - complex(x, y).abs) > 2.5);
        table[x + 15][y + 15] = '*';
    }

    writefln("%-(%s\n%)", table);
}
