import std.stdio, std.algorithm;

void hofstadterConwaySequence(in int m) {
    auto alist = new int[m + 1];
    alist[0 .. 2] = 1;
    auto v = alist[2];
    int k1 = 2, lg2 = 1;
    double amax = 0.0;

    foreach (n; 2 .. m + 1) {
        v = alist[n] = alist[v] + alist[n - v];
        amax = max(amax, v * 1.0 / n);
        if ((k1 & n) == 0) {
            writefln("Max in [2^%d, 2^%d]: %f", lg2, lg2 + 1, amax);
            amax = 0;
            lg2++;
        }
        k1 = n;
    }
}

void main() {
    hofstadterConwaySequence(2 ^^ 20);
}
