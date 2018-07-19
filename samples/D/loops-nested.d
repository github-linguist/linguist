import std.stdio, std.random;

void main() {
    int[10][10] mat;
    foreach (ref row; mat)
        foreach (ref item; row)
            item = uniform(1, 21);

    outer:
    foreach (row; mat)
        foreach (item; row) {
            write(item, ' ');
            if (item == 20)
                break outer;
        }

    writeln();
}
