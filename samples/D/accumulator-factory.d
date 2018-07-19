import std.stdio;

void main() {
    auto x = acc(1);
    x(5);
    acc(3);
    writeln(x(2.3));
}

auto acc(U = real, T)(T initvalue) { // U is type of the accumulator
    auto accum = cast(U)initvalue ;
    return (U n) { return accum += n ; } ;
}
