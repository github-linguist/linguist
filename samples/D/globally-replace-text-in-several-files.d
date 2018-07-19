import std.file, std.array;

void main() {
    auto from = "Goodbye London!", to = "Hello, New York!";
    foreach (fn; "a.txt b.txt c.txt".split()) {
        write(fn, replace(cast(string)read(fn), from, to));
    }
}
