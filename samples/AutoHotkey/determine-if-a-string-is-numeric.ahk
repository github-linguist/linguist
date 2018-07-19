list = 0 .14 -5.2 ten 0xf
Loop, Parse, list, %A_Space%
  MsgBox,% IsNumeric(A_LoopField)
Return

IsNumeric(x) {
  If x is number
    Return, 1
  Else Return, 0
}

;Output: 1  1  1  0  1
