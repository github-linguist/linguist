void main() {
    import std.stdio, std.algorithm, std.range;

    // Not true sets, items can be repeated, but must be sorted.
    auto s1 = [1, 2, 3, 4, 5, 6].assumeSorted;
    auto s2 = [2, 5, 6, 3, 4, 8].sort(); // [2,3,4,5,6,8].
    auto s3 = [1, 2, 5].assumeSorted;

    assert(s1.canFind(4)); // Linear search.
    assert(s1.contains(4)); // Binary search.
    assert(s1.setUnion(s2).equal([1,2,2,3,3,4,4,5,5,6,6,8]));
    assert(s1.setIntersection(s2).equal([2, 3, 4, 5, 6]));
    assert(s1.setDifference(s2).equal([1]));
    assert(s1.setSymmetricDifference(s2).equal([1, 8]));
    assert(s3.setDifference(s1).empty); // It's a subset.
    assert(!s1.equal(s2));

    auto s4 = [[1, 4, 7, 8], [1, 7], [1, 7, 8], [4], [7]];
    const s5 = [1, 1, 1, 4, 4, 7, 7, 7, 7, 8, 8];
    assert(s4.nWayUnion.equal(s5));
}
