import std.stdio, std.array;

void printCuboid(in int dx, in int dy, in int dz) {
    static cline(in int n, in int dx, in int dy, in string cde) {
        writef("%*s", n+1, cde[0 .. 1]);
        write(cde[1 .. 2].replicate(9*dx - 1));
        write(cde[0]);
        writefln("%*s", dy+1, cde[2 .. $]);
    }

    cline(dy+1, dx, 0, "+-");
    foreach (i; 1 .. dy+1)
        cline(dy-i+1, dx, i-1, "/ |");
    cline(0, dx, dy, "+-|");
    foreach (_; 0 .. 4*dz - dy - 2)
        cline(0, dx, dy, "| |");
    cline(0, dx, dy, "| +");
    foreach_reverse (i; 0 .. dy)
        cline(0, dx, i, "| /");
    cline(0, dx, 0, "+-\n");
}

void main() {
    printCuboid(2, 3, 4);
    printCuboid(1, 1, 1);
    printCuboid(6, 2, 1);
}
