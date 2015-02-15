import std.stdio, std.string;

void main() {
    const s = "the quick brown fox jumps over the lazy dog";
    enum n = 5, m = 3;

    writeln(s[n .. n + m]);

    writeln(s[n .. $]);

    writeln(s[0 .. $ - 1]);

    const i = s.indexOf("q");
    writeln(s[i .. i + m]);

    const j = s.indexOf("qu");
    writeln(s[j .. j + m]);
}
