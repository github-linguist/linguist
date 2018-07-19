import std.stdio, std.math, std.numeric;

T[][] cholesky(T)(in T[][] A) pure nothrow {
    auto L = new T[][](A.length, A.length);
    foreach (immutable r, row; L)
        row[r + 1 .. $] = 0;
    foreach (immutable i; 0 .. A.length)
        foreach (immutable j; 0 .. i + 1) {
            auto t = dotProduct(L[i][0 .. j], L[j][0 .. j]);
            L[i][j] = (i == j) ? (A[i][i] - t) ^^ 0.5 :
                                 (1.0 / L[j][j] * (A[i][j] - t));
        }
    return L;
}

void main() {
    double[][] m1 = [[25, 15, -5],
                     [15, 18,  0],
                     [-5,  0, 11]];
    writefln("%(%(%2.0f %)\n%)\n", cholesky(m1));

    double[][] m2 = [[18, 22,  54,  42],
                     [22, 70,  86,  62],
                     [54, 86, 174, 134],
                     [42, 62, 134, 106]];
    writefln("%(%(%2.3f %)\n%)\n", m2.cholesky);
}
