import std.stdio, std.file, std.string;

void main() {
    deleteLines("deleteline_test.txt", 1, 2);
}

void deleteLines(string name, int start, int num)
in {
    assert(start > 0, "Line counting must start at 1");
} body {
    start--;

    if (!exists(name) || !isFile(name))
        throw new FileException("File not found");

    auto lines = readText(name).splitLines();
    if (lines.length < start + num)
        throw new Exception("Can't delete lines past the end of file!");

    auto f = File(name, "w");
    foreach (int i, line; lines) {
        if (start > i || i >= start + num)
            f.writeln(line);
    }
}
