import std.traits;

T[][] matId(T)(in size_t n) pure nothrow if (isAssignable!(T, T)) {
    auto Id = new T[][](n, n);

    foreach (r, row; Id) {
        static if (__traits(compiles, {row[] = 0;})) {
            row[] = 0; // vector op doesn't work with T = BigInt
            row[r] = 1;
        } else {
            foreach (c; 0 .. n)
                row[c] = (c == r) ? 1 : 0;
        }
    }

    return Id;
}

void main() {
    import std.stdio, std.bigint;
    enum form = "[%([%(%s, %)],\n %)]]";

    immutable id1 = matId!real(5);
    writefln(form ~ "\n", id1);

    immutable id2 = matId!BigInt(3);
    writefln(form ~ "\n", id2);

    // auto id3 = matId!(const int)(2); // cant't compile
}
