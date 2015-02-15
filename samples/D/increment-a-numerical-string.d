void main() {
    import std.string;

    immutable s = "12349".succ;
    assert(s == "12350");
}
