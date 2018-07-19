; Originally submitted by Laszlo:
; http://www.autohotkey.com/forum/post-229412.html#229412

MsgBox % LuhnTest(49927398716)
MsgBox % LuhnTest(49927398717)
MsgBox % LuhnTest(1234567812345678)
MsgBox % LuhnTest(1234567812345670)

Return

;-----------------------------

LuhnTest(Number)
{
  MultFactor := 2 - ( StrLen(Number) & 1 )  ,  Sum := 0
  Loop, Parse, Number
    Sum += ( ( 9 < ( Temp := MultFactor * A_LoopField ) ) ? Temp - 9 : Temp )  ,  MultFactor := 3 - MultFactor
  Return !Mod(Sum,10)
}
