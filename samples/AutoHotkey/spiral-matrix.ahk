n := 5, dx := x := y := v := 1, dy := 0

Loop % n*n {
   a_%x%_%y% := v++
   nx := x+dx, ny := y+dy
   If (1 > nx || nx > n || 1 > ny || ny > n || a_%nx%_%ny%)
      t := dx, dx := -dy, dy := t
   x := x+dx, y := y+dy
}

Loop %n% {                      ; generate printout
   y := A_Index                 ; for each row
   Loop %n%                     ; and for each column
      s .= a_%A_Index%_%y% "`t" ; attach stored index
   s .= "`n"                    ; row is complete
}
MsgBox %s%                      ; show output
/*
---------------------------
1   2   3   4   5
16  17  18  19  6
15  24  25  20  7
14  23  22  21  8
13  12  11  10  9
---------------------------
*/
