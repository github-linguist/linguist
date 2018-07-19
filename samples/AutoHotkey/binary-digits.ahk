MsgBox % NumberToBinary(5) ;101
MsgBox % NumberToBinary(50) ;110010
MsgBox % NumberToBinary(9000) ;10001100101000

NumberToBinary(InputNumber)
{
 While, InputNumber
  Result := (InputNumber & 1) . Result, InputNumber >>= 1
 Return, Result
}
