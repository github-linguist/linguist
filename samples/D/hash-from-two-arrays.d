import std.array, std.range;

void main() {
    auto hash = ["a", "b", "c"].zip([1, 2, 3]).assocArray;
}
