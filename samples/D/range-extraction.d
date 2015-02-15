import std.stdio, std.conv, std.string, std.algorithm, std.range;

string rangeExtraction(in int[] items)
in {
    assert(items.isSorted);
} body {
    if (items.empty)
        return null;
    auto ranges = [[items[0].text]];

    foreach (immutable x, immutable y; items.zip(items[1 .. $]))
        if (x + 1 == y)
            ranges[$ - 1] ~= y.text;
        else
            ranges ~= [y.text];

    return ranges
           .map!(r => r.length > 2 ? r[0] ~ "-" ~ r.back : r.join(","))
           .join(",");
}

void main() {
    foreach (data; [[-8, -7, -6, -3, -2, -1, 0, 1, 3, 4, 5, 7, 8, 9,
                     10, 11, 14, 15, 17, 18, 19, 20],
                    [0, 0, 0, 1, 1],
                    [0, 1, 2, 4, 6, 7, 8, 11, 12, 14, 15, 16, 17, 18,
                     19, 20, 21, 22, 23, 24, 25, 27, 28, 29, 30, 31,
                     32, 33, 35, 36, 37, 38, 39]])
        data.rangeExtraction.writeln;
}
