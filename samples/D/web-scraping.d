void main() {
    import std.stdio, std.string, std.net.curl, std.algorithm;

    foreach (line; "http://tycho.usno.navy.mil/cgi-bin/timer.pl".byLine)
        if (line.canFind(" UTC"))
            line[4 .. $].writeln;
}
