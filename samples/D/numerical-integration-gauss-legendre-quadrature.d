import std.stdio, std.math;

immutable struct GaussLegendreQuadrature(size_t N, FP=double,
                                         size_t NBITS=50) {
    immutable static double[N] lroots, weight;
    alias FP[N + 1][N + 1] CoefMat;

    pure nothrow static this() {
        static FP legendreEval(in ref FP[N + 1][N + 1] lcoef,
                               in int n, in FP x) pure nothrow {
            FP s = lcoef[n][n];
            foreach_reverse (immutable i; 1 .. n+1)
                s = s * x + lcoef[n][i - 1];
            return s;
        }

        static FP legendreDiff(in ref CoefMat lcoef,
                               in int n, in FP x) pure nothrow {
            return n * (x * legendreEval(lcoef, n, x) -
                        legendreEval(lcoef, n - 1, x)) /
                   (x ^^ 2 - 1);
        }

        static void legendreRoots(in ref CoefMat lcoef) pure nothrow {
            foreach (immutable i; 1 .. N + 1) {
                FP x = cos(PI * (i - 0.25) / (N + 0.5));
                FP x1;
                do {
                    x1 = x;
                    x -= legendreEval(lcoef, N, x) /
                         legendreDiff(lcoef, N, x);
                } while (feqrel(x, x1) < NBITS);
                lroots[i - 1] = x;
                x1 = legendreDiff(lcoef, N, x);
                weight[i - 1] = 2 / ((1 - x ^^ 2) * (x1 ^^ 2));
            }
        }

        CoefMat lcoef = 0.0;
        legendreCoefInit(/*ref*/ lcoef);
        legendreRoots(lcoef);
    }

    static private void legendreCoefInit(ref CoefMat lcoef)
    pure nothrow {
        lcoef[0][0] = lcoef[1][1] = 1;
        foreach (immutable int n; 2 .. N + 1) { // n must be signed.
            lcoef[n][0] = -(n - 1) * lcoef[n - 2][0] / n;
            foreach (immutable i; 1 .. n + 1)
                lcoef[n][i] = ((2 * n - 1) * lcoef[n - 1][i - 1] -
                               (n - 1) * lcoef[n - 2][i]) / n;
        }
    }

    static public FP integrate(in FP function(FP x) f,
                               in FP a, in FP b) {
        immutable FP c1 = (b - a) / 2;
        immutable FP c2 = (b + a) / 2;
        FP sum = 0.0;
        foreach (immutable i; 0 .. N)
            sum += weight[i] * f(c1 * lroots[i] + c2);
        return c1 * sum;
    }
}

void main() {
    GaussLegendreQuadrature!(5, real) glq;
    writeln("Roots:  ", glq.lroots);
    writeln("Weight: ", glq.weight);
    writefln("Integrating exp(x) over [-3, 3]: %10.12f",
             glq.integrate(&exp, -3, 3));
    writefln("Compred to actual:               %10.12f",
             3.0.exp - -3.0.exp);
}
