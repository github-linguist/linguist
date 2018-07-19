import std.stdio;

void main() {
    immutable r = 7.125;
    writefln(" %9.3f",  -r);
    writefln(" %9.3f",   r);
    writefln(" %-9.3f",  r);
    writefln(" %09.3f", -r);
    writefln(" %09.3f",  r);
    writefln(" %-09.3f", r);
}
