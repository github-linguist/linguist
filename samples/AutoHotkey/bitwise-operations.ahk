bitwise(3, 4)
bitwise(a, b)
{
  MsgBox % "a and b: " . a & b
  MsgBox % "a or b: " . a | b
  MsgBox % "a xor b: " . a ^ b
  MsgBox % "not a: " . ~a       ; treated as unsigned integer
  MsgBox % "a << b: " . a << b  ; left shift
  MsgBox % "a >> b: " . a >> b  ; arithmetic right shift
}
