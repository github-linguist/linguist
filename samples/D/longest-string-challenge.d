import std.stdio, std.array;

/// Return a.length - b.length if positive, 0 otherwise.
int longer(string a, string b) {
    while (!a.empty && !b.empty)
        a.popFront(), b.popFront();
    return a.length;
}

void main() {
    string longest, lines;
    foreach (string line; stdin.lines())
        if (longer(line, longest))
            lines = longest = line;
        else if (!longer(longest, line))
            lines ~= line;

    writeln(lines);
}
