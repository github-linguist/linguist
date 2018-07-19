import std.stdio, std.algorithm, std.typecons, std.numeric,
       std.array, std.conv, std.string, std.range;

bool isRectangular(T)(in T[][] m) pure nothrow {
    return m.all!(r => r.length == m[0].length);
}

bool isSquare(T)(in T[][] m) pure nothrow {
    return m.isRectangular && m[0].length == m.length;
}

T[][] matrixMul(T)(in T[][] A, in T[][] B) pure nothrow
in {
    assert(A.isRectangular && B.isRectangular &&
           !A.empty && !B.empty && A[0].length == B.length);
} body {
    auto result = new T[][](A.length, B[0].length);
    auto aux = new T[B.length];

    foreach (immutable j; 0 .. B[0].length) {
        foreach (immutable k, const row; B)
            aux[k] = row[j];
        foreach (immutable i, const ai; A)
            result[i][j] = dotProduct(ai, aux);
    }

    return result;
}

/// Creates the pivoting matrix for m.
T[][] pivotize(T)(immutable T[][] m) pure nothrow
in {
    assert(m.isSquare);
} body {
    immutable n = m.length;
    auto id = iota(n)
              .map!((in j) => n.iota.map!(i => cast(T)(i == j)).array)
              .array;

    foreach (immutable i; 0 .. n) {
        // immutable row = iota(i, n).reduce!(max!(j => m[j][i]));
        T maxm = m[i][i];
        size_t row = i;
        foreach (immutable j; i .. n)
            if (m[j][i] > maxm) {
                maxm = m[j][i];
                row = j;
            }

        if (i != row)
            swap(id[i], id[row]);
    }

    return id;
}

/// Decomposes a square matrix A by PA=LU and returns L, U and P.
Tuple!(T[][],"L", T[][],"U", const T[][],"P")
lu(T)(immutable T[][] A) pure nothrow
in {
    assert(A.isSquare);
} body {
    immutable n = A.length;
    auto L = new T[][](n, n);
    auto U = new T[][](n, n);
    foreach (immutable i; 0 .. n) {
        L[i][i .. $] = 0;
        U[i][0 .. i] = 0;
    }

    immutable P = A.pivotize!T;
    immutable A2 = matrixMul!T(P, A);

    foreach (immutable j; 0 .. n) {
        L[j][j] = 1;
        foreach (immutable i; 0 .. j+1) {
            T s1 = 0;
            foreach (immutable k; 0 .. i)
                s1 += U[k][j] * L[i][k];
            U[i][j] = A2[i][j] - s1;
        }
        foreach (immutable i; j .. n) {
            T s2 = 0;
            foreach (immutable k; 0 .. j)
                s2 += U[k][j] * L[i][k];
            L[i][j] = (A2[i][j] - s2) / U[j][j];
        }
    }

    return typeof(return)(L, U, P);
}

void main() {
    immutable a = [[1.0, 3, 5],
                   [2.0, 4, 7],
                   [1.0, 1, 0]];
    immutable b = [[11.0, 9, 24, 2],
                   [1.0,  5,  2, 6],
                   [3.0, 17, 18, 1],
                   [2.0,  5,  7, 1]];

    auto f = "[%([%(%.1f, %)],\n %)]]\n\n".replicate(3);
    foreach (immutable m; [a, b])
        writefln(f, lu(m).tupleof);
}
