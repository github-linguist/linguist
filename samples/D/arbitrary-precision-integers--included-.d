void main() {
  import std.stdio, std.bigint, std.conv;

  auto s = text(5.BigInt ^^ 4 ^^ 3 ^^ 2);
  writefln("5^4^3^2 = %s..%s (%d digits)", s[0..20], s[$-20..$], s.length);
}
