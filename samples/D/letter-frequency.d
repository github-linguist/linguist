import std.stdio, std.ascii, std.algorithm, std.range, std.typetuple;

void main() {
    uint[26] frequency;

    foreach (const buffer; "unixdict.txt".File.byChunk(2 ^^ 15))
        foreach (immutable c; buffer.filter!isAlpha)
            frequency[c.toLower - 'a']++;

    writefln("%(%(%s, %),\n%)", frequency[].chunks(10));
}
