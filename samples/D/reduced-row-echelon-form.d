import std.stdio, std.algorithm, std.array, std.conv;

void toReducedRowEchelonForm(T)(T[][] M) pure nothrow {
    if (M.empty)
        return;
    immutable nrows = M.length;
    immutable ncols = M[0].length;

    size_t lead;
    foreach (r; 0 .. nrows) {
        if (ncols <= lead)
            return;
        {
            size_t i = r;
            while (M[i][lead] == 0) {
                i++;
                if (nrows == i) {
                    i = r;
                    lead++;
                    if (ncols == lead)
                        return;
                }
            }
            swap(M[i], M[r]);
        }

        M[r][] /= M[r][lead];
        foreach (j, ref mj; M)
            if (j != r)
                mj[] -= M[r][] * mj[lead];
        lead++;
    }
}

void main() {
    auto A = [[ 1, 2, -1,  -4],
              [ 2, 3, -1, -11],
              [-2, 0, -3,  22]];

    A.toReducedRowEchelonForm;
    writefln("%(%(%2d %)\n%)", A);
}
