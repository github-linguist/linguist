import std.stdio, std.typecons;

auto addSub(T)(T x, T y) {
    return tuple(x + y, x - y);
}

void main() {
    auto r = addSub(33, 12);
    writefln("33 + 12 = %d\n33 - 12 = %d", r.tupleof);
}
