import std.stdio, std.algorithm;

void wireworldStep(char[][] W1, char[][] W2) pure nothrow {
    foreach (r; 1 .. W1.length - 1)
        foreach (c; 1 .. W1[0].length - 1)
            switch (W1[r][c]) {
                case 'H': W2[r][c] = 't'; break;
                case 't': W2[r][c] = '.'; break;
                case '.':
                    int nH = 0;
                    foreach (sr; -1 .. 2)
                        foreach (sc; -1 .. 2)
                            nH += W1[r + sr][c + sc] == 'H';
                    W2[r][c] = (nH == 1 || nH == 2) ? 'H' : '.';
                    break;
                default:
            }
}

void main() {
    auto world = ["         ".dup,
                  "  tH     ".dup,
                  " .  .... ".dup,
                  "  ..     ".dup,
                  "         ".dup];

    char[][] world2;
    foreach (row; world)
        world2 ~= row.dup;

    foreach (step; 0 .. 7) {
        writefln("\nStep %d: ------------", step);
        foreach (row; world[1 .. $-1])
            writeln(row[1 .. $-1]);
        wireworldStep(world, world2);
        swap(world, world2);
    }
}
