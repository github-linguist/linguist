import std.stdio, std.file;

void main() {
    auto dir = "somedir";
    writeln(dir ~ " is empty: ", dirEmpty(dir));
}

bool dirEmpty(string dirname) {
    if (!exists(dirname) || !isDir(dirname))
        throw new Exception("dir not found: " ~ dirname);
    return dirEntries(dirname, SpanMode.shallow).empty;
}
