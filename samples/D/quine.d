const auto s=`const auto q="const auto s=\x60"~s~"\x60;
mixin(s);";import std.stdio;void main(){writefln(q);pragma(msg,q);}`;
mixin(s);
