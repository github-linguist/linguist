import std.stdio, std.utf;

void main() {
    string test = "a";
    size_t index = 0;

    // Get four-byte utf32 value for index 0
    // this returns dchar, so cast it to numeric.
    writeln(cast(uint)test.decode(index));

    // 'index' has moved to next character position in input.
    assert(index == 1);
}
