import std.stdio, std.algorithm;

void countingSort(int[] array, in size_t min, in size_t max)
pure nothrow {
    auto count = new int[max - min + 1];
    foreach (number; array)
        count[number - min]++;

    size_t z = 0;
    foreach (i; min .. max + 1)
        while (count[i - min] > 0) {
            array[z] = i;
            z++;
            count[i - min]--;
        }
}

void main() {
    auto data = [9, 7, 10, 2, 9, 7, 4, 3, 10, 2, 7, 10, 2, 1, 3, 8,
                 7, 3, 9, 5, 8, 5, 1, 6, 3, 7, 5, 4, 6, 9, 9, 6, 6,
                 10, 2, 4, 5, 2, 8, 2, 2, 5, 2, 9, 3, 3, 5, 7, 8, 4];

    int dataMin = reduce!min(data);
    int dataMax = reduce!max(data);
    countingSort(data, dataMin, dataMax);
    assert(isSorted(data));
}
