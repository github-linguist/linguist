import std.stdio, std.string;

void main() {
    auto s = " \t \r \n String with spaces  \t  \r  \n  ";
    assert(s.stripLeft() == "String with spaces  \t  \r  \n  ");
    assert(s.stripRight() == " \t \r \n String with spaces");
    assert(s.strip() == "String with spaces");
}
