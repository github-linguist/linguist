OpenConsole()
; Fill arrays
Dim a.s(2)
Dim b.s(2)
Dim c(2)
For Arrayposition = 0 To ArraySize(a())
  a(Arrayposition) = Chr(Asc("a") + Arrayposition)
  b(Arrayposition) = Chr(Asc("A") + Arrayposition)
  c(Arrayposition) = Arrayposition + 1
Next
; loop over them
For Arrayposition = 0 To ArraySize(a())
  PrintN(a(Arrayposition) + b(Arrayposition) + Str(c(Arrayposition)))
Next
Input() ;wait for Enter before ending
