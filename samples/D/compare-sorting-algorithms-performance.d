import std.stdio, std.algorithm, std.container, std.datetime,
       std.random, std.typetuple;

immutable int[] allOnes, sortedData, randomData;

static this() { // Initialize global Arrays.
    immutable size_t arraySize = 10_000;

    allOnes = new int[arraySize];
    //allOnes[] = 1;
    foreach (ref d; allOnes)
        d = 1;

    sortedData = new int[arraySize];
    foreach (immutable i, ref d; sortedData)
        d = i;

    randomData = new int[arraySize];
    foreach (ref d; randomData)
        d = uniform(0, int.max);
}

// BubbleSort:

void bubbleSort(T)(T[] list) {
    for (int i = list.length - 1; i > 0; i--)
        for (int j = i -1; j >= 0; j--)
            if (list[i] < list[j])
                swap(list[i], list[j]);
}

void allOnesBubble() {
    auto data = allOnes.dup;
    data.bubbleSort;
    assert(data.isSorted);
}

void sortedBubble() {
    auto data = sortedData.dup;
    data.bubbleSort;
    assert(data.isSorted);
}

void randomBubble() {
    auto data = randomData.dup;
    data.bubbleSort;
    assert(data.isSorted);
}

// InsertionSort:

void insertionSort(T)(T[] list) {
    foreach (immutable i, currElem; list) {
        size_t j = i;
        for (; j > 0 && currElem < list[j - 1]; j--)
            list[j] = list[j - 1];
        list[j] = currElem;
    }
}

void allOnesInsertion() {
    auto data = allOnes.dup;
    data.insertionSort;
    assert(data.isSorted);
}

void sortedInsertion() {
    auto data = sortedData.dup;
    data.insertionSort;
    assert(data.isSorted);
}

void randomInsertion() {
    auto data = randomData.dup;
    data.insertionSort;
    assert(data.isSorted);
}

// HeapSort:

void heapSort(T)(T[] data) {
    auto h = data.heapify;
    while (!h.empty)
        h.removeFront;
}

void allOnesHeap() {
    auto data = allOnes.dup;
    data.heapSort;
    assert(data.isSorted);
}

void sortedHeap() {
    auto data = sortedData.dup;
    data.heapSort;
    assert(data.isSorted);
}

void randomHeap() {
    auto data = randomData.dup;
    data.heapSort;
    assert(data.isSorted);
}

// Built-in sort:

void allOnesBuiltIn() {
    auto data = allOnes.dup;
    data.sort!q{a < b};
    assert(data.isSorted);
}

void sortedBuiltIn() {
    auto data = sortedData.dup;
    data.sort!q{a < b};
    assert(data.isSorted);
}

void randomBuiltIn() {
    auto data = randomData.dup;
    data.sort!q{a < b};
    assert(data.isSorted);
}

static void show(in TickDuration[4u] r) {
    alias args = TypeTuple!("usecs", int);
    writefln("    Bubble Sort:    %10d", r[0].to!args);
    writefln("    Insertion Sort: %10d", r[1].to!args);
    writefln("    Heap Sort:      %10d", r[3].to!args);
    writefln("    Built-in Sort:  %10d", r[2].to!args);
}

void main() {
    enum nRuns = 100;
    writeln("Timings in microseconds:");

    writeln("  Testing against all ones:");
    nRuns.benchmark!(allOnesBubble, allOnesInsertion,
                     allOnesHeap, allOnesBuiltIn).show;

    writeln("\n  Testing against sorted data.");
    nRuns.benchmark!(sortedBubble, sortedInsertion,
                     sortedHeap, sortedBuiltIn).show;

    writeln("\n  Testing against random data.");
    nRuns.benchmark!(randomBubble, randomInsertion,
                     randomHeap, randomBuiltIn).show;
}
