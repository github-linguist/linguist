import std.stdio, std.algorithm;

void bubbleSort(T)(T[] data) pure nothrow {
    auto itemCount = data.length;
    bool hasChanged = false;

    do {
        hasChanged = false;
        itemCount--;
        foreach (immutable i; 0 .. itemCount)
            if (data[i] > data[i + 1]) {
                swap(data[i], data[i + 1]);
                hasChanged = true;
            }
    } while (hasChanged);
}

void main() {
    auto array = [28, 44, 46, 24, 19, 2, 17, 11, 25, 4];
    array.bubbleSort();
    writeln(array);
}
