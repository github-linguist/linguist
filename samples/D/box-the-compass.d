import std.stdio, std.string, std.math, std.array;

struct boxTheCompass {
    immutable static string[32] points;

    pure nothrow static this() {
        immutable cardinal = ["north", "east", "south", "west"];
        immutable desc = ["1", "1 by 2", "1-C", "C by 1", "C",
                          "C by 2", "2-C", "2 by 1"];

        foreach (immutable i; 0 .. 4) {
            immutable s1 = cardinal[i];
            immutable s2 = cardinal[(i + 1) % 4];
            immutable sc = (s1 == "north" || s1 == "south") ?
                           (s1 ~ s2) : (s2 ~ s1);
            foreach (immutable j; 0 .. 8)
                points[i * 8 + j] = desc[j].replace("1", s1).
                                    replace("2", s2).replace("C", sc);
        }
    }

    static string opCall(in double degrees) pure /*nothrow*/ {
        immutable testD = (degrees / 11.25) + 0.5;
        return points[cast(int)floor(testD % 32)].capitalize;
    }
}

void main() {
    foreach (immutable i; 0 .. 33) {
        immutable heading = i * 11.25 + [0, 5.62, -5.62][i % 3];
        writefln("%s\t%18s\t%s", i % 32 + 1,
                 heading.boxTheCompass, heading);
    }
}
