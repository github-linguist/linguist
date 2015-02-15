import std.stdio: writeln;

void main() {
    for (int i = 10; i >= 0; --i)
        writeln(i);
    writeln();

    foreach_reverse (i ; 0 .. 10 + 1)
        writeln(i);
}
