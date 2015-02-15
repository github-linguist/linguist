T rot(T)(in T x, in int shift) pure nothrow {
    return (x >>> shift) | (x << (T.sizeof * 8 - shift));
}

void testBit(in int a, in int b) {
  import std.stdio;
  writefln("Input: a = %d, b = %d", a, b);
  writefln("AND  : %8b  & %08b = %032b (%4d)", a, b, a & b, a & b);
  writefln(" OR  : %8b  | %08b = %032b (%4d)", a, b, a | b, a | b);
  writefln("XOR  : %8b  ^ %08b = %032b (%4d)", a, b, a ^ b, a ^ b);
  writefln("LSH  : %8b << %08b = %032b (%4d)", a, b, a << b, a << b);
  writefln("RSH  : %8b >> %08b = %032b (%4d)", a, b, a >> b, a >> b);
  writefln("NOT  : %8s  ~ %08b = %032b (%4d)", "", a, ~a, ~a);
  writefln("ROT  : rot(%8b, %d)     = %032b (%4d)",
           a, b, rot(a, b), rot(a, b));
}

void main() {
  immutable int a = 0b_1111_1111; // bit literal 255
  immutable int b = 0b_0000_0010; // bit literal 2

  testBit(a, b);
}
