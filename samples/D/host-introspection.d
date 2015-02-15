void main() {
  import std.stdio, std.system;

  writeln("Word size = ", size_t.sizeof * 8, " bits.");
  writeln(endian == Endian.littleEndian ? "Little" : "Big", " endian.");
}
