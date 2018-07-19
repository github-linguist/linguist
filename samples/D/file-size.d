import std.file, std.stdio, std.path, std.file, std.stream,
       std.mmfile;

void main() {
    immutable fileName = "file_size.exe";

    try {
        writefln("File '%s' has size:", fileName);

        writefln("%10d bytes by std.file.getSize (function)",
                 std.file.getSize(fileName));

        writefln("%10d bytes by std.stream (class)",
                 new std.stream.File(fileName).size);

        // mmfile can treat the file as an array in memory.
        writefln("%10d bytes by std.mmfile (class)",
                 new std.mmfile.MmFile(fileName).length);
    } catch (Exception e) {
        e.msg.writefln;
    }
}
