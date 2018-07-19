import std.stdio, std.string, std.math, std.algorithm, std.traits;

/**
Bin the answers to fn() and check bin counts are within
+/- delta % of repeats/bincount.
*/
void distCheck(TF)(in TF func, in int nRepeats, in double delta)
if (isCallable!TF) {
    int[int] counts;
    foreach (immutable i; 0 .. nRepeats)
        counts[func()]++;
    immutable double target = nRepeats / double(counts.length);
    immutable int deltaCount = cast(int)(delta / 100.0 * target);

    foreach (k, count; counts)
        if (abs(target - count) >= deltaCount)
            throw new Exception(format(
                "distribution potentially skewed for '%s': '%d'\n",
                k, count));

    foreach (k; counts.keys.sort())
        writeln(k, " ", counts[k]);
    writeln;
}

version (verify_distribution_uniformity_naive_main) {
    void main() {
        import std.random;
        distCheck(() => uniform(1, 6), 1_000_000, 1);
    }
}
