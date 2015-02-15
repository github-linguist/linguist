import std.stdio, std.algorithm, std.random;

void bogoSort(T)(T[] data) {
    while (!isSorted(data))
        randomShuffle(data);
}

void main() {
    auto array = [2, 7, 41, 11, 3, 1, 6, 5, 8];
    bogoSort(array);
    writeln(array);
}
