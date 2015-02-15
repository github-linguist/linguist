void main() {
    import std.stdio, std.regex;

    immutable s = "I am a string";

    // Test.
    if (!s.match(r"string$").empty)
        "Ends with 'string'.".writeln;

    // Substitute.
    s.replace(" a ".regex, " another ").writeln;
}
