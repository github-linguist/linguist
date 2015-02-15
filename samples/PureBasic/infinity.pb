If OpenConsole()
  Define.d a, b
  b = 0

  ;positive infinity
  PrintN(StrD(Infinity())) ;returns the value for positive infinity from builtin function

  a = 1.0
  PrintN(StrD(a / b)) ;calculation results in the value of positive infinity

  ;negative infinity
  PrintN(StrD(-Infinity())) ;returns the value for negative infinity from builtin function

  a = -1.0
  PrintN(StrD(a / b)) ;calculation results in the value of negative infinity

  Print(#crlf$ + #crlf$ + "Press ENTER to exit"): Input()
  CloseConsole()
EndIf
