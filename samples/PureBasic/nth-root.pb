#Def_p=0.001

Procedure.d Nth_root(n.i, A.d, p.d=#Def_p)
  Protected Dim x.d(1)
  x(0)=A: x(1)=A/n
  While Abs(x(1)-x(0))>p
    x(0)=x(1)
    x(1)=((n-1.0)*x(1)+A/Pow(x(1),n-1.0))/n
  Wend
  ProcedureReturn x(1)
EndProcedure

;//////////////////////////////
Debug "125'th root of 5642 is"
Debug Pow(5642,1/125)
Debug "First estimate is:"
Debug Nth_root(125,5642)
Debug "And better:"
Debug Nth_root(125,5642,0.00001)
