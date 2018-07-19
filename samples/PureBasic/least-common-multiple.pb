Procedure GCDiv(a, b); Euclidean algorithm
  Protected r
  While b
    r = b
    b = a%b
    a = r
  Wend
  ProcedureReturn a
EndProcedure

Procedure LCM(m,n)
  Protected t
  If m And n
    t=m*n/GCDiv(m,n)
  EndIf
  ProcedureReturn t*Sign(t)
EndProcedure
