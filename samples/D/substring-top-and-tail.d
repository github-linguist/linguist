import std.stdio;

void main() {
    // strip first character
    writeln("knight"[1 .. $]);

    // strip last character
    writeln("socks"[0 .. $ - 1]);

    // strip both first and last characters
    writeln("brooms"[1 .. $ - 1]);
}
