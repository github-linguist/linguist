OpenConsole()
For a = 1 To 22
  ; Integer, so no floor needed
  tmp = 1 / 2 + Sqr(a)
  Print(Str(a + tmp) + ", ")
Next
PrintN("")
PrintN("Starting check till one million")
For a = 1 To 1000000
  value.d = a + Round((1 / 2 + Sqr(a)), #PB_Round_Down)
  root    = Sqr(value)
  If value - root*root = 0
    found + 1
    If found < 20
      PrintN("Found a square! " + Str(value))
    ElseIf found = 20
      PrintN("And more")
    EndIf
  EndIf
Next
If found
  PrintN(Str(found) + " Squares found, see above")
Else
  PrintN("No squares, all ok")
EndIf
; Wait for enter
Input()
