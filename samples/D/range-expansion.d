import std.stdio, std.regex, std.conv, std.range, std.algorithm;

enum rangeEx = (string s) /*pure*/ => s.matchAll(`(-?\d+)-?(-?\d+)?,?`)
    .map!q{ a[1].to!int.iota(a[1 + !a[2].empty].to!int + 1) }.join;

void main() {
    "-6,-3--1,3-5,7-11,14,15,17-20".rangeEx.writeln;
}
