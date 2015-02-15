import std.c.stdio;

void recurse(in uint i=0) {
    printf("%u ", i);
    recurse(i + 1);
}

void main() {
    recurse();
}
