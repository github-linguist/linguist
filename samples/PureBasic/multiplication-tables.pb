Procedure PrintMultiplicationTable(maxx, maxy)
  sp       = Len(Str(maxx*maxy)) + 1
  trenner$ =  "+"
  For l1 = 1 To maxx + 1
    For l2 = 1 To sp
      trenner$ + "-"
    Next
    trenner$ + "+"
  Next
  header$ = "|" + RSet("x", sp) + "|"
  For a = 1 To maxx
    header$ + RSet(Str(a), sp)
    header$ + "|"
  Next
  PrintN(trenner$)
  PrintN(header$)
  PrintN(trenner$)
  For y = 1 To maxy
    line$ = "|" + RSet(Str(y), sp) + "|"
    For x = 1 To maxx
      If x >= y
        line$ + RSet(Str(x*y), sp)
      Else
        line$ + Space(sp)
      EndIf
      line$ + "|"
    Next
    PrintN(line$)
  Next
  PrintN(trenner$)
EndProcedure

OpenConsole()
PrintMultiplicationTable(12, 12)
Input()
