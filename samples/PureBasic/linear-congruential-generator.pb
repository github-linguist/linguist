Procedure ms_LCG(seed.q = -1)
  Static state.q
  If seed >= 0
    state = seed
  Else
    state = (state * 214013 + 2531011) % (1 << 31)
    ProcedureReturn state >> 16
  EndIf
EndProcedure

Procedure.q bsd_LCG(seed.q = -1)
  Static state.q
  If seed >= 0
    state = seed
  Else
    state = (state * 1103515245 + 12345) % (1 << 31)
    ProcedureReturn state
  EndIf
EndProcedure

If OpenConsole()
  Define i
  PrintN("BSD (seed = 1)")
  bsd_LCG(1)
  For i = 1 To 5
    PrintN(Str(bsd_LCG()))
  Next

  PrintN(#CRLF$ + "MS (seed = 1)")
  ms_LCG(1)
  For i = 1 To 5
    PrintN(Str(ms_LCG()))
  Next

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit"): Input()
  CloseConsole()
EndIf
