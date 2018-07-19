T[] deconv(T)(in T[] g, in T[] f) pure nothrow {
    int flen = f.length;
    int glen = g.length;
    auto result = new T[glen - flen + 1];
    foreach (int n, ref e; result) {
        e = g[n];
        immutable lowerBound = (n >= flen) ? n - flen + 1 : 0;
        foreach (i; lowerBound .. n)
                e -= result[i] * f[n - i];
        e /= f[0];
    }
    return result;
}

void main() {
    import std.stdio;
    immutable h = [-8,-9,-3,-1,-6,7];
    immutable f = [-3,-6,-1,8,-6,3,-1,-9,-9,3,-2,5,2,-2,-7,-1];
    immutable g = [24,75,71,-34,3,22,-45,23,245,25,52,25,-67,
                   -96,96,31,55,36,29,-43,-7];
    writeln(deconv(g, f) == h, " ", deconv(g, f));
    writeln(deconv(g, h) == f, " ", deconv(g, h));
}
