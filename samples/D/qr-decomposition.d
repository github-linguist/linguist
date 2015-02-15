import std.stdio, std.math, std.algorithm, std.traits,
       std.typecons, std.numeric, std.range, std.conv;

template elementwiseMat(string op) {
    T[][] elementwiseMat(T, U)(in T[][] A, in U B)
    pure nothrow if (is(U == T) || is(U == T[][])) {
        static if (is(U == T[][]))
            assert(A.length == B.length);
        if (A.empty)
            return null;
        auto R = new typeof(return)(A.length, A[0].length);

        foreach (immutable r, const row; A)
            static if (is(U == T)) {
                R[r][] = mixin("row[] " ~ op ~ "B");
            } else {
                assert(row.length == B[r].length);
                R[r][] = mixin("row[] " ~ op ~ "B[r][]");
            }

        return R;
    }
}

alias msum = elementwiseMat!q{ + },
      msub = elementwiseMat!q{ - },
      pmul = elementwiseMat!q{ * },
      pdiv = elementwiseMat!q{ / };

bool isRectangular(T)(in T[][] mat) pure nothrow {
    return mat.all!(r => r.length == mat[0].length);
}

T[][] matMul(T)(in T[][] a, in T[][] b) pure nothrow
in {
    assert(a.isRectangular && b.isRectangular &&
           a[0].length == b.length);
} body {
    auto result = new T[][](a.length, b[0].length);
    auto aux = new T[b.length];
    foreach (immutable j; 0 .. b[0].length) {
        foreach (immutable k; 0 .. b.length)
            aux[k] = b[k][j];
        foreach (immutable i; 0 .. a.length)
            result[i][j] = a[i].dotProduct(aux);
    }
    return result;
}

Unqual!T[][] transpose(T)(in T[][] m) pure nothrow {
    auto r = new Unqual!T[][](m[0].length, m.length);
    foreach (immutable nr, const row; m)
        foreach (immutable nc, immutable c; row)
            r[nc][nr] = c;
    return r;
}

T norm(T)(in T[][] m) pure nothrow {
    return transversal(m, 0).map!q{ a ^^ 2 }.sum.sqrt;
}

T[][] makeUnitVector(T)(in size_t dim) pure nothrow {
    auto result = new T[][](dim, 1);
    foreach (row; result)
        row[] = 0;
    result[0][0] = 1;
    return result;
}

/// Return a nxn identity matrix.
T[][] matId(T)(in size_t n) pure nothrow {
    auto Id = new T[][](n, n);
    foreach (immutable r, row; Id) {
        row[] = 0;
        row[r] = 1;
    }
    return Id;
}

Unqual!T[][] slice2D(T)(in T[][] A,
                        in size_t ma, in size_t mb,
                        in size_t na, in size_t nb) pure nothrow {
    auto B = new Unqual!T[][](mb - ma + 1, nb - na + 1);
    foreach (immutable i, brow; B)
        brow[] = A[ma + i][na .. na + brow.length];
    return B;
}

size_t rows(T)(in T[][] A) pure nothrow { return A.length; }

size_t cols(T)(in T[][] A) pure nothrow {
    return A.length ? A[0].length : 0;
}

T[][] mcol(T)(in T[][] A, in size_t n) pure nothrow {
    return slice2D(A, 0, rows(A)-1, n, n);
}

T[][] matEmbed(T)(in T[][] A, in T[][] B,
                  in size_t row, in size_t col) pure nothrow {
    auto C = new T[][](rows(A), cols(A));
    foreach (immutable i, const arow; A)
        C[i][] = arow[]; // Some wasted copies.
    foreach (immutable i, const brow; B)
        C[row + i][col .. col + brow.length] = brow[];
    return C;
}

// Main routines ---------------

T[][] makeHouseholder(T)(in T[][] a) {
    immutable size_t m = rows(a);
    immutable T s = sgn(a[0][0]);
    immutable e = makeUnitVector!T(m);
    immutable u = msum(a, pmul(e, norm(a) * s));
    immutable v = pdiv(u, u[0][0]);
    immutable beta = 2.0 / matMul(transpose(v), v)[0][0];
    return msub(matId!T(m), pmul(matMul(v, transpose(v)), beta));
}

Tuple!(T[][],"Q", T[][],"R") QRdecomposition(T)(T[][] A) {
    immutable m = A.rows;
    immutable n = A.cols;
    auto Q = matId!T(m);

    // Work on n columns of A.
    foreach (immutable i; 0 .. (m == n ? n-1 : n)) {
        // Select the i-th submatrix. For i=0 this means the original
        // matrix A.
        immutable B = slice2D(A, i, m-1, i, n-1);

        // Take the first column of the current submatrix B.
        immutable x = mcol(B, 0);

        // Create the Householder matrix for the column and embed it
        // into an mxm identity.
        immutable H = matEmbed(matId!T(m), makeHouseholder(x), i, i);

        // The product of all H matrices from the right hand side is
        // the orthogonal matrix Q.
        Q = matMul(Q, H);

        // The product of all H matrices with A from the LHS is the
        // upper triangular matrix R.
        A  = matMul(H, A);
    }

    // Return Q and R.
    return typeof(return)(Q, A);
}

// Polynomial regression ---------------

/// Solve an upper triangular system by back substitution.
T[][] solveUpperTriangular(T)(in T[][] R, in T[][] b) pure nothrow {
    immutable size_t n = cols(R);
    auto x = new T[][](n, 1);

    foreach_reverse (immutable k; 0 .. n) {
        T tot = 0;
        foreach (immutable j; k + 1 .. n)
            tot += R[k][j] * x[j][0];
        x[k][0] = (b[k][0] - tot) / R[k][k];
    }

    return x;
}

/// Solve a linear least squares problem by QR decomposition.
T[][] lsqr(T)(T[][] A, in T[][] b) pure nothrow {
    const qr = QRdecomposition(A);
    immutable size_t n = cols(qr.R);
    return solveUpperTriangular(
        slice2D(qr.R, 0, n-1, 0, n-1),
        slice2D(matMul(transpose(qr.Q), b), 0, n-1, 0, 0));
}

Unqual!T[][] polyFit(T)(in T[][] x, in T[][] y, in size_t n)
pure nothrow {
    immutable size_t m = cols(x);
    auto A = new Unqual!T[][](m, n + 1);
    foreach (immutable i, row; A)
        foreach (immutable j, ref item; row)
            item = x[0][i] ^^ j;
    return lsqr(A, transpose(y));
}

void main() {
    // const (Q, R) = QRdecomposition(
    const qr = QRdecomposition([[12.0, -51,   4],
                                [ 6.0, 167, -68],
                                [-4.0,  24, -41]]);
    immutable string form = "[%([%(%2.3f, %)]%|,\n %)]\n";
    writefln(form, qr.Q);
    writefln(form, qr.R);

    immutable x = [[0.0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]];
    immutable y = [[1.0, 6, 17, 34, 57, 86, 121, 162, 209, 262, 321]];
    writeln(polyFit(x, y, 2));
}
