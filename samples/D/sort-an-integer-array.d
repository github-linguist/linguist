import std.stdio, std.algorithm;

void main() {
    auto data = [2, 4, 3, 1, 2];
    data.sort(); // in-place
    assert(data == [1, 2, 2, 3, 4]);
}
