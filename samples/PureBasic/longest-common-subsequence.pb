Procedure.s lcs(a$, b$)
  Protected x$ , lcs$
  If Len(a$) = 0 Or Len(b$) = 0
    lcs$ = ""
  ElseIf Right(a$, 1) = Right(b$, 1)
    lcs$ = lcs(Left(a$, Len(a$) - 1), Left(b$, Len(b$) - 1)) + Right(a$, 1)
  Else
    x$ = lcs(a$, Left(b$, Len(b$) - 1))
    y$ = lcs(Left(a$, Len(a$) - 1), b$)
    If Len(x$) > Len(y$)
      lcs$ = x$
    Else
      lcs$ = y$
    EndIf
  EndIf
  ProcedureReturn lcs$
EndProcedure
OpenConsole()
PrintN( lcs("thisisatest", "testing123testing"))
PrintN("Press any key to exit"): Repeat: Until Inkey() <> ""
