import std.stdio;

struct Subtractive {
    enum MOD = 1_000_000_000;
    private int[55] state;
    private int si, sj;

    this(in int p1) pure nothrow {
        subrandSeed(p1);
    }

    void subrandSeed(int p1) pure nothrow {
        int p2 = 1;

        state[0] = p1 % MOD;
        for (int i = 1, j = 21; i < 55; i++, j += 21) {
            if (j >= 55)
                j -= 55;
            state[j] = p2;
            if ((p2 = p1 - p2) < 0)
                p2 += MOD;
            p1 = state[j];
        }

        si = 0;
        sj = 24;
        foreach (i; 0 .. 165)
            subrand();
    }

    int subrand() pure nothrow {
        if (si == sj)
            subrandSeed(0);

        if (!si--)
            si = 54;
        if (!sj--)
            sj = 54;

        int x = state[si] - state[sj];
        if (x < 0)
            x += MOD;

        return state[si] = x;
    }
}

void main() {
    auto gen = Subtractive(292_929);
    foreach (i; 0 .. 10)
        writeln(gen.subrand());
}
