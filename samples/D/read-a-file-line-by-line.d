import std.stdio;

void main() {
    foreach (line; File("unixdict.txt").byLine())
        writeln(line);
}
