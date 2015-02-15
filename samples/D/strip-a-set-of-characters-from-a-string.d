import std.stdio, std.string;

void main() {
    auto s = "She was a soul stripper. She took my heart!";
    auto ss = "Sh ws  soul strppr. Sh took my hrt!";
    assert(s.removechars("aei") == ss);
}
