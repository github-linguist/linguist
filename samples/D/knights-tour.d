import std.stdio, std.algorithm, std.random, std.range,
       std.conv, std.typecons, std.typetuple;

int[N][N] knightTour(size_t N=8)(in string start)
in {
    assert(start.length >= 2);
} body {
    static struct P { int x, y; }

    immutable P[8] moves = [P(2,1), P(1,2), P(-1,2), P(-2,1),
                            P(-2,-1), P(-1,-2), P(1,-2), P(2,-1)];
    int[N][N] data;

    int[8] sortMoves(in int x, in int y) {
        int[2][8] counts;
        foreach (immutable i, immutable ref d1; moves) {
            int c = 0;
            foreach (immutable ref d2; moves) {
                immutable p = P(x + d1.x + d2.x, y + d1.y + d2.y);
                if (p.x >= 0 && p.x < N && p.y >= 0 && p.y < N &&
                    data[p.y][p.x] == 0)
                    c++;
            }
            counts[i] = [c, i];
        }

        counts[].randomShuffle; // Shuffle to randomly break ties.
        counts[].sort(); // Lexicographic sort.

        int[8] result = void;
        transversal(counts[], 1).copy(result[]);
        return result;
    }

    immutable p0 = P(start[0] - 'a', N - to!int(start[1 .. $]));
    data[p0.y][p0.x] = 1;

    Tuple!(int, int, int, int[8])[N * N] order;
    order[0] = tuple(p0.x, p0.y, 0, sortMoves(p0.x, p0.y));

    int n = 0;
    while (n < (N * N - 1)) {
        immutable int x = order[n][0];
        immutable int y = order[n][1];
        bool ok = false;
        foreach (immutable i; order[n][2] .. 8) {
            immutable P d = moves[order[n][3][i]];
            if (x+d.x < 0 || x+d.x >= N || y+d.y < 0 || y+d.y >= N)
                continue;

            if (data[y + d.y][x + d.x] == 0) {
                order[n][2] = i + 1;
                n++;
                data[y + d.y][x + d.x] = n + 1;
                order[n] = tuple(x+d.x,y+d.y,0,sortMoves(x+d.x,y+d.y));
                ok = true;
                break;
            }
        }

        if (!ok) { // Failed. Backtrack.
            data[y][x] = 0;
            n--;
        }
    }

    return data;
}

void main() {
    foreach (immutable i, side; TypeTuple!(5, 8, 31, 101)) {
        immutable form = "%(%" ~ text(side ^^ 2).length.text ~ "d %)";
        foreach (ref row; ["c3", "b5", "a1", "a1"][i].knightTour!side)
            writefln(form, row);
        writeln();
    }
}
