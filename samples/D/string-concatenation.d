import std.stdio;

void main() {
    string s = "hello";
    writeln(s ~ " world");
    auto s2 = s ~ " world";
    writeln(s2);
}
