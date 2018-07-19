import std.stdio, std.array, std.range, std.traits;

/// Recursive.
bool binarySearch(R, T)(/*in*/ R data, in T x)
pure nothrow if (isRandomAccessRange!R &&
                 is(Unqual!T == Unqual!(ElementType!R))) {
    if (data.empty)
        return false;
    immutable i = data.length / 2;
    immutable mid = data[i];
    if (mid > x)
        return data[0 .. i].binarySearch(x);
    if (mid < x)
        return data[i + 1 .. $].binarySearch(x);
    return true;
}

/// Iterative.
bool binarySearchIt(R, T)(/*in*/ R data, in T x)
pure nothrow if (isRandomAccessRange!R &&
                 is(Unqual!T == Unqual!(ElementType!R))) {
    while (!data.empty) {
        immutable i = data.length / 2;
        immutable mid = data[i];
        if (mid > x)
            data = data[0 .. i];
        else if (mid < x)
            data = data[i + 1 .. $];
        else
            return true;
    }
    return false;
}

void main() {
    /*const*/ auto items = [2, 4, 6, 8, 9].assumeSorted;
    foreach (x; [1, 8, 10, 9, 5, 2])
        writefln("%2d %5s %5s %5s", x,
                 items.binarySearch(x),
                 items.binarySearchIt(x),
                 // Standard Binary Search:
                 !items.equalRange(x).empty);
}
