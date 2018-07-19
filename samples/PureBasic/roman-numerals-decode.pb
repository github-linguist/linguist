Procedure romanDec(roman.s)
  Protected i, n, lastval, arabic

  For i = Len(roman) To 1 Step -1
    Select UCase(Mid(roman, i, 1))
      Case "M"
        n = 1000
      Case "D"
        n = 500
      Case "C"
        n = 100
      Case "L"
        n = 50
      Case "X"
        n = 10
      Case "V"
        n = 5
      Case "I"
        n = 1
      Default
        n = 0
    EndSelect
    If (n < lastval)
      arabic - n
    Else
      arabic + n
    EndIf
    lastval = n
  Next

  ProcedureReturn arabic
EndProcedure

If OpenConsole()
  PrintN(Str(romanDec("MCMXCIX"))) ;1999
  PrintN(Str(romanDec("MDCLXVI"))) ;1666
  PrintN(Str(romanDec("XXV")))     ;25
  PrintN(Str(romanDec("CMLIV")))   ;954
  PrintN(Str(romanDec("MMXI")))    ;2011

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit"): Input()
  CloseConsole()
EndIf
