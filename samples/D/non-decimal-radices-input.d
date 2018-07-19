import std.stdio, std.conv;

void main() {
    immutable text = "100";
    foreach (base; 2 .. 21)
        writefln("String '%s' in base %d is  %d in base 10" ,
                 text, base, to!int(text, base));
}
