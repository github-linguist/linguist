import std.algorithm, std.range, std.traits, permutations2,
       permutations_by_swapping1;

auto prod(Range)(Range r) /*pure*/ nothrow {
    return reduce!q{a * b}(cast(ForeachType!Range)1, r);
}

T permanent(T)(in T[][] a) /*pure nothrow*/
in {
    foreach (const row; a)
        assert(row.length == a[0].length);
} body {
    auto r = a.length.iota;
    T tot = 0;
    foreach (sigma; r.array.permutations)
        tot += r.map!(i => a[i][sigma[i]]).prod;
    return tot;
}

T determinant(T)(in T[][] a) /*pure nothrow*/
in {
    foreach (const row; a)
        assert(row.length == a[0].length);
} body {
    immutable n = a.length;
    auto r = n.iota;
    T tot = 0;
    //foreach (sigma, sign; n.spermutations) {
    foreach (sigma_sign; n.spermutations) {
        const sigma = sigma_sign[0];
        immutable sign = sigma_sign[1];
        tot += sign * r.map!(i => a[i][sigma[i]]).prod;
    }
    return tot;
}

void main() {
    import std.stdio;

    foreach (/*immutable*/ const a; [[[1, 2],
                                      [3, 4]],

                                     [[1, 2, 3, 4],
                                      [4, 5, 6, 7],
                                      [7, 8, 9, 10],
                                      [10, 11, 12, 13]],

                                     [[ 0,  1,  2,  3,  4],
                                      [ 5,  6,  7,  8,  9],
                                      [10, 11, 12, 13, 14],
                                      [15, 16, 17, 18, 19],
                                      [20, 21, 22, 23, 24]]]) {
        writefln("[%([%(%2s, %)],\n %)]]", a);
        writefln("Permanent: %s, determinant: %s\n",
                 a.permanent, a.determinant);
    }
}
