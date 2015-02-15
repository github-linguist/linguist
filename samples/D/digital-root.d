import std.stdio, std.typecons, std.conv, std.bigint, std.math,
       std.traits;

Tuple!(uint, Unqual!T) digitalRoot(T)(in T inRoot, in uint base)
pure /*nothrow*/
in {
    assert(base > 1);
} body {
    Unqual!T root = inRoot.abs;
    uint persistence = 0;
    while (root >= base) {
        auto num = root;
        root = 0;
        while (num != 0) {
            root += num % base;
            num /= base;
        }
        persistence++;
    }
    return typeof(return)(persistence, root);
}

void main() {
    enum f1 = "%s(%d): additive persistance= %d, digital root= %d";
    foreach (immutable b; [2, 3, 8, 10, 16, 36]) {
        foreach (immutable n; [5, 627615, 39390, 588225, 393900588225])
            writefln(f1, text(n, b), b, n.digitalRoot(b)[]);
        writeln;
    }

    enum f2 = "<BIG>(%d): additive persistance= %d, digital root= %d";
    immutable n = BigInt("581427189816730304036810394583022044713" ~
                         "00738980834668522257090844071443085937");
    foreach (immutable b; [2, 3, 8, 10, 16, 36])
        writefln(f2, b, n.digitalRoot(b)[]); // Shortened output.
}
