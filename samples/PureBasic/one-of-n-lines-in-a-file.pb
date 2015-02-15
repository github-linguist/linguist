Procedure.f randomFloat()
   ProcedureReturn Random(1000000) / 1000000
EndProcedure

Procedure one_of_n(n)
  Protected linesRead, lineChosen
  While linesRead < n
    linesRead + 1
    If randomFloat() <= (1.0 / (linesRead))
       lineChosen = linesRead
    EndIf
  Wend
  ProcedureReturn lineChosen
EndProcedure

If OpenConsole()
  #testFileLineCount = 10
  #simulationCount = 1000000
  Define i
  Dim a(#testFileLineCount) ;index 0 is not used
  For i = 1 To #simulationCount
    x = one_of_n(#testFileLineCount)
    a(x) + 1
  Next

  For i = 1 To #testFileLineCount
    Print(Str(a(i)) + "  ")
  Next
  PrintN("")

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit"): Input()
  CloseConsole()
EndIf
