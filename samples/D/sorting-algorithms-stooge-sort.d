import std.stdio, std.algorithm, std.array;

void stoogeSort(T)(T[] seq) pure nothrow {
    if (seq.back < seq.front)
        swap(seq.front, seq.back);

    if (seq.length > 2) {
        immutable m = seq.length / 3;
        seq[0 .. $ - m].stoogeSort();
        seq[m .. $].stoogeSort();
        seq[0 .. $ - m].stoogeSort();
    }
}

void main() {
    auto data = [1, 4, 5, 3, -6, 3, 7, 10, -2, -5];
    data.stoogeSort();
    writeln(data);
}
