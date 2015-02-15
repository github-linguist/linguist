n := 8, a := 8*atan(1)/n
Loop %n%
   i := A_Index-1, t .= cos(a*i) ((s:=sin(a*i))<0 ? " - i*" . -s : " + i*" . s) "`n"
Msgbox % t
