import std.stdio, std.string, std.algorithm;

void main() {
    auto s = "abcd";

    /* Comparing two strings for exact equality */
    assert (s == "abcd"); // same object

    /* Comparing two strings for inequality */
    assert(s != "ABCD"); // different objects

    /* Comparing the lexical order of two strings;
    -1 means smaller, 0 means equal, 1 means larger */

    assert(s.icmp("Bcde") == -1); // case insensitive
    assert(s.cmp("Bcde") == 1); // case sensitive

    assert(s.icmp("Aabc") == 1); // case insensitive
    assert(s.cmp("Aabc") == 1); // case sensitive

    assert(s.icmp("ABCD") == 0); // case insensitive
    assert(s.cmp("ABCD") == 1); // case sensitive
}
