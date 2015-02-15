Procedure Kaprekar(n.i)
  nn.q  = n*n
  tens.q= 1
  While tens<nn: tens*10: Wend
  Repeat
    tens/10
    If tens<=n: Break: EndIf
    If nn-n = (nn/tens) * (tens-1)
      ProcedureReturn #True
    EndIf
  ForEver
  If n=1
    ProcedureReturn #True
  EndIf
EndProcedure

If OpenConsole()
  For i=1 To 1000000
    If Kaprekar(i)
      cnt+1
      PrintN(RSet(Str(cnt),3)+":"+RSet(Str(i),8))
    EndIf
  Next
  ;
  Print("Press ENTER to exit")
  Input()
EndIf
