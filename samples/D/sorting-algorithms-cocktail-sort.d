import std.stdio, std.algorithm;

void cocktailSort(Range)(Range data) {
    bool swapped = false;
    do {
        foreach (i; 0 .. data.length - 1)
            if (data[i] > data[i + 1]) {
                swap(data[i], data[i + 1]);
                swapped = true;
            }
        if (!swapped)
            break;
        swapped = false;
        foreach_reverse (i; 0 .. data.length - 1)
            if (data[i] > data[i + 1]) {
                swap(data[i], data[i + 1]);
                swapped = true;
            }
    } while(swapped);
}

void main() {
    auto array = ["John", "Kate", "Zerg", "Alice", "Joe", "Jane"];
    cocktailSort(array);
    writeln(array);
}
