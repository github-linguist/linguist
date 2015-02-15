; saving the division for last ensures we divide the largest
; numerator by the smallest denominator

Procedure.q CatalanNumber(n.q)
If n<0:ProcedureReturn 0:EndIf
If n=0:ProcedureReturn 1:EndIf
ProcedureReturn (2*(2*n-1))*CatalanNumber(n-1)/(n+1)
EndProcedure

ls=25
rs=12

a.s=""
a.s+LSet(RSet("n",rs),ls)+"CatalanNumber(n)"
; cw(a.s)
Debug a.s

For n=0 to 33 ;33 largest correct quad for n
a.s=""
a.s+LSet(RSet(Str(n),rs),ls)+Str(CatalanNumber(n))
; cw(a.s)
Debug a.s
Next
