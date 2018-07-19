import std.stdio, std.random, std.math, std.algorithm, std.range;

real analytical(in int n) /*pure nothrow*/ {
    enum aux = (int k)=> reduce!q{a * b}(1.0L, iota(n - k + 1, n + 1));
    return iota(1, n + 1)
           .map!(k => (aux(k) * k ^^ 2) / (real(n) ^^ (k + 1)))
           .sum;
}

size_t loopLength(size_t maxN)(in int size, ref Xorshift rng) {
    __gshared static bool[maxN + 1] seen;
    seen[0 .. size + 1] = false;
    int current = 1;
    size_t steps = 0;
    while (!seen[current]) {
        seen[current] = true;
        current = uniform(1, size + 1, rng);
        steps++;
    }
    return steps;
}

void main() {
    enum maxN  = 40;
    enum nTrials = 300_000;
    auto rng = Xorshift(unpredictableSeed);
    writeln(" n    average    analytical     (error)");
    writeln("===  =========  ============  ==========");

    foreach (immutable n; 1 .. maxN + 1) {
        long total = 0;
        foreach (immutable _; 0 .. nTrials)
            total += loopLength!maxN(n, rng);
        immutable average = total / real(nTrials);
        immutable an = n.analytical;
        immutable percentError = abs(an - average) / an * 100;
        immutable errorS = format("%2.4f", percentError);
        writefln("%3d  %9.5f  %12.5f  (%7s%%)",
                 n, average, an, errorS);
    }
}
