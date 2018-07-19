import std.stdio, std.random;

void main() {
    while (true) {
        int r = uniform(0, 20);
        write(r, " ");
        if (r == 10)
            break;
        write(uniform(0, 20), " ");
    }
}
