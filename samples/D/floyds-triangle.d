import std.stdio, std.conv;

void floydTriangle(in uint n) {
    immutable lowerLeftCorner = n * (n - 1) / 2 + 1;
    foreach (r; 0 .. n)
        foreach (c; 0 .. r + 1)
            writef("%*d%c",
                   text(lowerLeftCorner + c).length,
                   r * (r + 1) / 2 + c + 1,
                   c == r ? '\n' : ' ');
}

void main() {
    floydTriangle(5);
    floydTriangle(14);
}
