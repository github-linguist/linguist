import std.stdio, std.range, std.algorithm, std.typecons, std.array;

Tuple!(double[], double[]) polyDiv(in double[] inN, in double[] inD)
pure /*nothrow*/ {
    // Code smell: a function that does two things.
    static int trimAndDegree(T)(ref T[] poly) nothrow pure {
        poly = poly.retro.find!q{ a != b }(0.0).retro;
        return (cast(int)poly.length) - 1;
    }

    double[] N = inN.dup; // Not nothrow.
    const(double)[] D = inD;
    const dD = trimAndDegree(D);
    auto dN = trimAndDegree(N);
    double[] q;
    if (dD < 0)
        throw new Error("ZeroDivisionError");
    if (dN >= dD) {
        q = [0.0].replicate(dN);
        while (dN >= dD) {
            auto d = [0.0].replicate(dN - dD) ~ D;
            immutable mult = q[dN - dD] = N[$ - 1] / d[$ - 1];
            d[] *= mult;
            N[] -= d[];
            dN = trimAndDegree(N);
        }
    } else
        q = [0.0];
    return tuple(q, N);
}


int trimAndDegree1(T)(ref T[] poly) nothrow pure {
    poly.length -= poly.retro.countUntil!q{ a != 0 };
    return (cast(int)poly.length) - 1;
}

void main() {
    immutable N = [-42.0, 0.0, -12.0, 1.0];
    immutable D = [-3.0, 1.0, 0.0, 0.0];
    writefln("%s / %s = %s  remainder %s", N, D, polyDiv(N, D)[]);
}
