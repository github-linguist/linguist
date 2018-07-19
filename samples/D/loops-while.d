import std.stdio;

void main() {
    int i = 1024;

    while (i > 0) {
        writeln(i);
        i >>= 1;
    }
}
