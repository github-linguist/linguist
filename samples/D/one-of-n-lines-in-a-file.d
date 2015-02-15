import std.stdio, std.random, std.algorithm;

// Zero-based line numbers.
int oneOfN(in int n) {
    int choice = 0;
    foreach (immutable i; 1 .. n)
        if (!uniform(0, i + 1))
            choice = i;
    return choice;
}

void main() {
    int[10] bins;
    foreach (immutable i; 0 .. 1_000_000)
        bins[10.oneOfN]++;

    bins.writeln;
    writeln("Total of bins: ", bins[].sum);
}
