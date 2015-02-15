Enumeration
  #Composite
  #Probably_prime
EndEnumeration

Procedure Miller_Rabin(n, k)
  Protected d=n-1, s, x, r
  If n=2
    ProcedureReturn #Probably_prime
  ElseIf n%2=0 Or n<2
    ProcedureReturn #Composite
  EndIf
  While d%2=0
    d/2
    s+1
  Wend
  While k>0
    k-1
    x=Int(Pow(2+Random(n-4),d))%n
    If x=1 Or x=n-1: Continue: EndIf
    For r=1 To s-1
      x=(x*x)%n
      If x=1: ProcedureReturn #Composite: EndIf
      If x=n-1: Break: EndIf
    Next
    If x<>n-1: ProcedureReturn #Composite: EndIf
  Wend
  ProcedureReturn #Probably_prime
EndProcedure
