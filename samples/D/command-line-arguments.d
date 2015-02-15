void main(in string[] args) {
    import std.stdio;

    foreach (immutable i, arg; args[1 .. $])
        writefln("#%2d : %s", i + 1, arg);
}
