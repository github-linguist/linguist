OpenConsole()

For i.i = 1 To 10
  Print(Str(i))
  If i % 5 = 0
    PrintN("")
    Continue
  EndIf
  Print(",")
Next

Repeat: Until Inkey() <> ""
