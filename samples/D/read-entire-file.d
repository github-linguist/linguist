import std.file: read, readText;

void main() {
    // To read a whole file into a dynamic array of unsigned bytes:
    auto data = cast(ubyte[])read("unixdict.txt");

    // To read a whole file into a validated UTF-8 string:
    string txt = readText("unixdict.txt");
}
