Procedure hq9plus(code.s)
  Protected accumulator, i, bottles
  For i = 1 To Len(code)
    Select Mid(code, i, 1)
      Case "h", "H"
        PrintN("Hello, world!")
      Case "q", "Q"
        PrintN(code)
      Case "9"
        bottles = 99
        While bottles
          PrintN(Str(bottles) + " bottles of beer on the wall, " + Str(bottles) + " bottles of beer,")
          bottles - 1
          PrintN("Take one down, pass it around, " + Str(bottles) + " bottles of beer on the wall.")
        Wend
      Case "+"
        accumulator + 1
    EndSelect
  Next i
EndProcedure

If OpenConsole()
  Define testCode.s = "hq9+HqQ+Qq"
  hq9plus(testCode)

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit"): Input()
  CloseConsole()
EndIf
