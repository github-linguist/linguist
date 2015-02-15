Prototype cond(Array t(1))

Enumeration #Null
  #Baker
  #Cooper
  #Fletcher
  #Miller
  #Smith
EndEnumeration

Procedure checkTenands(Array tenants(1), Array Condions.cond(1))
  Protected i, j
  Protected.cond *f
  j=ArraySize(Condions())
  For i=0 To j
    *f=Condions(i)              ; load the function pointer to the current condition
    If *f(tenants()) = #False
      ProcedureReturn  #False
    EndIf
  Next
  ProcedureReturn #True
EndProcedure

Procedure C1(Array t(1))
  If Int(Abs(t(#Fletcher)-t(#Cooper)))<>1
    ProcedureReturn #True
  EndIf
EndProcedure

Procedure C2(Array t(1))
  If t(#Baker)<>5
    ProcedureReturn #True
  EndIf
EndProcedure

Procedure C3(Array t(1))
  If t(#Cooper)<>1
    ProcedureReturn #True
  EndIf
EndProcedure

Procedure C4(Array t(1))
  If t(#Miller) >= t(#Cooper)
    ProcedureReturn #True
  EndIf
EndProcedure

Procedure C5(Array t(1))
  If t(#Fletcher)<>1 And t(#Fletcher)<>5
    ProcedureReturn #True
  EndIf
EndProcedure

Procedure C6(Array t(1))
  If Int(Abs(t(#Smith)-t(#Fletcher)))<>1
    ProcedureReturn #True
  EndIf
EndProcedure


If OpenConsole()
  Dim People(4)
  Dim Conditions(5)
  Define a, b, c, d, e, i
  ;
  ;- Load all conditions
  Conditions(i)=@C1(): i+1
  Conditions(i)=@C2(): i+1
  Conditions(i)=@C3(): i+1
  Conditions(i)=@C4(): i+1
  Conditions(i)=@C5(): i+1
  Conditions(i)=@C6()
  ;
  ; generate and the all legal combinations
  For a=1 To 5
    For b=1 To 5
      If a=b: Continue: EndIf
      For c=1 To 5
        If a=c Or b=c: Continue: EndIf
        For d=1 To 5
          If d=a Or d=b Or d=c : Continue: EndIf
          For e=1 To 5
            If e=a Or e=b Or e=c Or e=d: Continue: EndIf
            People(#Baker)=a
            People(#Cooper)=b
            People(#Fletcher)=c
            People(#Miller)=d
            People(#Smith)=e
            If checkTenands(People(), Conditions())
              PrintN("Solution found;")
              PrintN("Baker="+Str(a)+#CRLF$+"Cooper="+Str(b)+#CRLF$+"Fletcher="+Str(c))
              PrintN("Miller="+Str(d)+#CRLF$+"Smith="+Str(e)+#CRLF$)
            EndIf
          Next
        Next
      Next
    Next
  Next
  Print("Press ENTER to exit"): Input()
EndIf
