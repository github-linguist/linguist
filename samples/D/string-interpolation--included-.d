void main() {
    import std.stdio, std.string;

    "Mary had a %s lamb.".format("little").writeln;
    "Mary had a %2$s %1$s lamb.".format("little", "white").writeln;
}
