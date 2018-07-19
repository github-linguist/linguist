void main() {
    import std.stdio, std.range, std.algorithm, std.datetime;

    writeln("Christmas comes on a Sunday in the years:\n",
            iota(2008, 2122)
            .filter!(y => Date(y, 12, 25).dayOfWeek == DayOfWeek.sun));
}
