import std.stdio, std.file, std.path;

void verify(in string name) {
    if (name.exists())
        writeln("'", name, "' exists");
    else
        writeln("'", name, "' doesn't exist");
}

void main() {
    // check in current working dir
    verify("input.txt");
    verify("docs");

    // check in root
    verify(dirSeparator ~ "input.txt");
    verify(dirSeparator ~ "docs");
}
