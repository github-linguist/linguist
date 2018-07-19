import std.stdio, std.math, std.algorithm;

bool nearZero(T)(in T a, in T b = T.epsilon * 4) pure nothrow {
    return abs(a) <= b;
}

T[] findRoot(T)(immutable T function(in T) pure nothrow fi,
                in T start, in T end, in T step=T(0.001L),
                T tolerance = T(1e-4L)) {
    if (step.nearZero)
        writefln("WARNING: step size may be too small.");

    /// Search root by simple bisection.
    T searchRoot(T a, T b) pure nothrow {
        T root;
        int limit = 49;
        T gap = b - a;

        while (!nearZero(gap) && limit--) {
            if (fi(a).nearZero)
                return a;
            if (fi(b).nearZero)
                return b;
            root = (b + a) / 2.0L;
            if (fi(root).nearZero)
                return root;
            ((fi(a) * fi(root) < 0) ? b : a) = root;
            gap = b - a;
        }

        return root;
    }

    immutable dir = T(end > start ? 1.0 : -1.0);
    immutable step2 = (end > start) ? abs(step) : -abs(step);
    T[T] result;
    for (T x = start; (x * dir) <= (end * dir); x += step2)
        if (fi(x) * fi(x + step2) <= 0) {
            immutable T r = searchRoot(x, x + step2);
            result[r] = fi(r);
        }

    return result.keys.sort().release;
}

void report(T)(in T[] r, immutable T function(in T) pure f,
               in T tolerance = T(1e-4L)) {
    if (r.length) {
        writefln("Root found (tolerance = %1.4g):", tolerance);

        foreach (const x; r) {
            immutable T y = f(x);

            if (nearZero(y))
                writefln("... EXACTLY at %+1.20f, f(x) = %+1.4g",x,y);
            else if (nearZero(y, tolerance))
                writefln(".... MAY-BE at %+1.20f, f(x) = %+1.4g",x,y);
            else
                writefln("Verify needed, f(%1.4g) = " ~
                         "%1.4g > tolerance in magnitude", x, y);
        }
    } else
        writefln("No root found.");
}

void main() {
    static real f(in real x) pure nothrow {
        return x ^^ 3 - (3 * x ^^ 2) + 2 * x;
    }

    findRoot(&f, -1.0L, 3.0L, 0.001L).report(&f);
}
