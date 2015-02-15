import std.stdio, std.container, std.array, std.typecons;

void main() {
    alias tuple T;
    auto heap = heapify([T(3, "Clear drains"),
                         T(4, "Feed cat"),
                         T(5, "Make tea"),
                         T(1, "Solve RC tasks"),
                         T(2, "Tax return")]);

    while (!heap.empty) {
        writeln(heap.front);
        heap.removeFront();
    }
}
