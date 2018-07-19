LCM(Number1,Number2)
{
 If (Number1 = 0 || Number2 = 0)
  Return
 Var := Number1 * Number2
 While, Number2
  Num := Number2, Number2 := Mod(Number1,Number2), Number1 := Num
 Return, Var // Number1
}

Num1 = 12
Num2 = 18
MsgBox % LCM(Num1,Num2)
