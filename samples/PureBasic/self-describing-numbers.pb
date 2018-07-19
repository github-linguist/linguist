Procedure isSelfDescribing(x.q)
  ;returns 1 if number is self-describing, otherwise it returns 0
  Protected digitCount, digit, i, digitSum
  Dim digitTally(10)
  Dim digitprediction(10)

  If x <= 0
    ProcedureReturn 0 ;number must be positive and non-zero
  EndIf

  While x > 0 And i < 10
    digit = x % 10
    digitSum + digit
    If digitSum > 10
      ProcedureReturn 0 ;sum of digits' values exceeds maximum possible
    EndIf
    digitprediction(i) = digit
    digitTally(digit) + 1
    x / 10
    i + 1
  Wend
  digitCount = i - 1

  If digitSum < digitCount Or x > 0
    ProcedureReturn 0  ;sum of digits' values is too small or number has more than 10 digits
  EndIf

  For i = 0 To digitCount
    If digitTally(i) <> digitprediction(digitCount - i)
      ProcedureReturn 0 ;number is not self-describing
    EndIf
  Next
  ProcedureReturn 1 ;number is self-describing
EndProcedure

Procedure displayAll()
  Protected i, j, t
  PrintN("Starting search for all self-describing numbers..." + #CRLF$)
  For j = 0 To 9
    PrintN(#CRLF$ + "Searching possibilites " + Str(j * 1000000000) + " -> " + Str((j + 1) * 1000000000 - 1)+ "...")
    t = ElapsedMilliseconds()
    For i = 0 To 999999999
      If isSelfDescribing(j * 1000000000 + i)
        PrintN(Str(j * 1000000000 + i))
      EndIf
    Next
    PrintN("Time to search this range of possibilities: " + Str((ElapsedMilliseconds() - t) / 1000) + "s.")
  Next
  PrintN(#CRLF$ + "Search complete.")
EndProcedure

If OpenConsole()

  DataSection
    Data.q 1210, 2020, 21200, 3211000, 42101000, 521001000, 6210001000, 3214314
  EndDataSection

  Define i, x.q
  For i = 1 To 8
    Read.q x
    Print(Str(x) + " is ")
    If Not isSelfDescribing(x)
      Print("not ")
    EndIf
    PrintN("selfdescribing.")
  Next
  PrintN(#CRLF$)

  displayAll()

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit"): Input()
  CloseConsole()
EndIf
