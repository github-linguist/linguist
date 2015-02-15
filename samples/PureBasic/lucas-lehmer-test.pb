Procedure Lucas_Lehmer_Test(p)
  Protected mp.q = (1 << p) - 1, sn.q = 4, i
  For i = 3 To p
    sn = (sn * sn - 2) % mp
  Next
  If sn = 0
    ProcedureReturn #True
  EndIf
  ProcedureReturn #False
EndProcedure

#upperBound = SizeOf(Quad) * 8 - 1 ;equivalent to significant bits in a signed quad integer
If OpenConsole()
  Define p = 3
  PrintN("M2")
  While p <= #upperBound
    If Lucas_Lehmer_Test(p)
      PrintN("M" + Str(p))
    EndIf
    p + 2
  Wend

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit"): Input()
  CloseConsole()
EndIf
