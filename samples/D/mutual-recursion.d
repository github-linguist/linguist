import std.stdio, std.algorithm, std.range;

int male(in int n) pure nothrow {
    return n ? n - male(n - 1).female : 0;
}

int female(in int n) pure nothrow {
    return n ? n - female(n - 1).male : 1;
}

void main() {
    20.iota.map!female.writeln;
    20.iota.map!male.writeln;
}
