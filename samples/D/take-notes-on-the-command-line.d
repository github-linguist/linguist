void main(in string[] args) {
    import std.stdio, std.file, std.datetime, std.range;

    immutable filename = "NOTES.TXT";

    if (args.length == 1) {
        if (filename.exists && filename.isFile)
            writefln("%-(%s\n%)", filename.File.byLine);
    } else {
        auto f = File(filename, "a+");
        f.writefln("%s", cast(DateTime)Clock.currTime);
        f.writefln("\t%-(%s %)", args.dropOne);
    }
}
