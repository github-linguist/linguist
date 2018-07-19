import std.stdio;

class K {}

void main() {
    K k;
    if (k is null)
        writeln("k is null");
    k = new K;
    if (k !is null)
        writeln("Now k is not null");
}
