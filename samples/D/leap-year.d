import std.algorithm;

bool leapYear(in uint y) pure nothrow {
    return (y % 4) == 0 && (y % 100 || (y % 400) == 0);
}

void main() {
    auto good = [1600, 1660, 1724, 1788, 1848, 1912, 1972, 2032,
                 2092, 2156, 2220, 2280, 2344, 2348];
    auto bad =  [1698, 1699, 1700, 1750, 1800, 1810, 1900, 1901,
                 1973, 2100, 2107, 2200, 2203, 2289];
    assert(filter!leapYear(bad ~ good).equal(good));
}
