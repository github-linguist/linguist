void insertionSort(T)(T[] data) pure nothrow {
    foreach (i, value; data[1 .. $]) {
        auto j = i + 1;
        for ( ; j > 0 && value < data[j - 1]; j--)
            data[j] = data[j - 1];
        data[j] = value;
    }
}

void main() {
    import std.stdio;
    auto items = [28, 44, 46, 24, 19, 2, 17, 11, 25, 4];
    items.insertionSort();
    writeln(items);
}
