import std.stdio;

struct LinearCongruentialGenerator {
    enum uint RAND_MAX = (1U << 31) - 1;
    uint seed = 0;

    uint randBSD() pure nothrow {
        seed = (seed * 1_103_515_245 + 12_345) & RAND_MAX;
        return seed;
    }

    uint randMS() pure nothrow {
        seed = (seed * 214_013 + 2_531_011) & RAND_MAX;
        return seed >> 16;
    }
}

void main() {
    LinearCongruentialGenerator rnd;

    foreach (i; 0 .. 10)
        writeln(rnd.randBSD());
    writeln();

    rnd.seed = 0;
    foreach (i; 0 .. 10)
        writeln(rnd.randMS());
}
