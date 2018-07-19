import std.stdio;

void main() {
    int val;
    do {
        val++;
        write(val, " ");
    } while (val % 6 != 0);
}
