Structure RR
  a.f
  b.f
EndStructure

Procedure.f MapRange(*a.RR, *b.RR, s)
  Protected.f a1, a2, b1, b2
  a1=*a\a:  a2=*a\b
  b1=*b\a:  b2=*b\b
  ProcedureReturn b1 + ((s - a1) * (b2 - b1) / (a2 - a1))
EndProcedure


;- Test the function
If OpenConsole()
  Define.RR Range1, Range2
  Range1\a=0: Range1\b=10
  Range2\a=-1:Range2\b=0
  ;
  For i=0 To 10
    PrintN(RSet(Str(i),2)+" maps to "+StrF(MapRange(@Range1, @Range2, i),1))
  Next
EndIf
