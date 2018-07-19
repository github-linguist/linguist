Procedure is_Perfect_number(n)
  Protected summa, i=1, result=#False
  Repeat
    If Not n%i
      summa+i
    EndIf
    i+1
  Until i>=n
  If summa=n
    result=#True
  EndIf
  ProcedureReturn result
EndProcedure
