import std.stdio, std.math, std.mathspecial;

real taylorGamma(in real x) pure nothrow {
    static immutable real[30] table = [
     0x1p+0,                    0x1.2788cfc6fb618f4cp-1,
    -0x1.4fcf4026afa2dcecp-1,  -0x1.5815e8fa27047c8cp-5,
     0x1.5512320b43fbe5dep-3,  -0x1.59af103c340927bep-5,
    -0x1.3b4af28483e214e4p-7,   0x1.d919c527f60b195ap-8,
    -0x1.317112ce3a2a7bd2p-10, -0x1.c364fe6f1563ce9cp-13,
     0x1.0c8a78cd9f9d1a78p-13, -0x1.51ce8af47eabdfdcp-16,
    -0x1.4fad41fc34fbb2p-20,    0x1.302509dbc0de2c82p-20,
    -0x1.b9986666c225d1d4p-23,  0x1.a44b7ba22d628acap-28,
     0x1.57bc3fc384333fb2p-28, -0x1.44b4cedca388f7c6p-30,
     0x1.cae7675c18606c6p-34,   0x1.11d065bfaf06745ap-37,
    -0x1.0423bac8ca3faaa4p-38,  0x1.1f20151323cd0392p-41,
    -0x1.72cb88ea5ae6e778p-46, -0x1.815f72a05f16f348p-48,
     0x1.6198491a83bccbep-50,  -0x1.10613dde57a88bd6p-53,
     0x1.5e3fee81de0e9c84p-60,  0x1.a0dc770fb8a499b6p-60,
    -0x1.0f635344a29e9f8ep-62,  0x1.43d79a4b90ce8044p-66];

    immutable real y = x - 1.0L;
    real sm = table[$ - 1];
    foreach_reverse (immutable an; table[0 .. $ - 1])
        sm = sm * y + an;
    return 1.0L / sm;
}

real lanczosGamma(real z) pure nothrow {
    // Coefficients used by the GNU Scientific Library.
    // http://en.wikipedia.org/wiki/Lanczos_approximation
    enum g = 7;
    static immutable real[9] table =
        [    0.99999_99999_99809_93,
           676.52036_81218_851,
         -1259.13921_67224_028,
           771.32342_87776_5313,
          -176.61502_91621_4059,
            12.50734_32786_86905,
            -0.13857_10952_65720_12,
             9.98436_95780_19571_6e-6,
             1.50563_27351_49311_6e-7];

    // Reflection formula.
    if (z < 0.5L) {
        return PI / (sin(PI * z) * lanczosGamma(1 - z));
    } else {
        z -= 1;
        real x = table[0];
        foreach (immutable i; 1 .. g + 2)
            x += table[i] / (z + i);
        immutable real t = z + g + 0.5L;
        return sqrt(2 * PI) * t ^^ (z + 0.5L) * exp(-t) * x;
    }
}

void main() {
    foreach (immutable i; 1 .. 11) {
        immutable real x = i / 3.0L;
        writefln("%f: %20.19e %20.19e %20.19e", x,
                 x.taylorGamma, x.lanczosGamma, x.gamma);
    }
}
