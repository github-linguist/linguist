import std.file, std.exception;

void truncateFile(in string name, in size_t newSize) {
    if (!exists(name) || !isFile(name))
        throw new Exception("File not found.");

    auto size = getSize(name);
    if (size <= newSize)
        throw new Exception(
            "New size must be smaller than original size.");

    auto content = cast(ubyte[])read(name, newSize);
    if (content.length != newSize)
        throw new Exception("Reading file failed.");

    write(name, content);
    enforce(getSize(name) == newSize);
}

void main() {
    truncateFile("truncate_test.txt", 0);
}
