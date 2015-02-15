Loop 22
   t .= (A_Index + floor(0.5 + sqrt(A_Index))) "  "
MsgBox %t%

s := 0
Loop 1000000
   x := A_Index + floor(0.5 + sqrt(A_Index)), s += x = round(sqrt(x))**2
Msgbox Number of bad squares = %s% ; 0
