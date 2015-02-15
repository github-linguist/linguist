Procedure.d AGM(a.d, g.d, ErrLim.d=1e-15)
  Protected.d ta=a+1, tg
  While ta <> a
    ta=a: tg=g
    a=(ta+tg)*0.5
    g=Sqr(ta*tg)
  Wend
  ProcedureReturn a
EndProcedure

If OpenConsole()
  PrintN(StrD(AGM(1, 1/Sqr(2)), 16))
  Input()
  CloseConsole()
EndIf
