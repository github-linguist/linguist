import std.stdio, std.algorithm, std.range, std.functional;

auto aMean(T)(T data) {
    return data.sum / data.length;
}

auto gMean(T)(T data) pure {
    return data.reduce!q{a * b} ^^ (1.0 / data.length);
}

auto hMean(T)(T data) pure {
    return data.length / data.reduce!q{ 1.0 / a + b };
}

void main() {
    immutable m = [adjoin!(hMean, gMean, aMean)(iota(1.0L, 11.0L))[]];
    writefln("%(%.19f %)", m);
    assert(m.isSorted);
}
