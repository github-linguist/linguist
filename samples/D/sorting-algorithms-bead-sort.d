import std.stdio, std.algorithm, std.range, std.array, std.functional;

alias repeat0 = curry!(repeat, 0);

// Currenty std.range.transposed doesn't work.
auto columns(R)(R m) /*pure nothrow*/ {
    return m
           .map!walkLength
           .reduce!max
           .iota
           .map!(i => m.filter!(s => s.length > i).walkLength.repeat0);
}

auto beadSort(in uint[] data) /*pure nothrow*/ {
    return data.map!repeat0.columns.columns.map!walkLength;
}

void main() {
    [5, 3, 1, 7, 4, 1, 1].beadSort.writeln;
}
