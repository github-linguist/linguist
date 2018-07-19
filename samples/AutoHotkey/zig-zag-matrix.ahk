n = 5                           ; size
v := x := y := 1                ; initial values
Loop % n*n {                    ; for every array element
   a_%x%_%y% := v++             ; assign the next index
   If ((x+y)&1)                 ; odd diagonal
      If (x < n)                ; while inside the square
         y -= y<2 ? 0 : 1, x++  ; move right-up
      Else y++                  ; on the edge increment y, but not x: to even diagonal
   Else                         ; even diagonal
      If (y < n)                ; while inside the square
         x -= x<2 ? 0 : 1, y++  ; move left-down
      Else x++                  ; on the edge increment x, but not y: to odd diagonal
}

Loop %n% {                      ; generate printout
   x := A_Index                 ; for each row
   Loop %n%                     ; and for each column
      t .= a_%x%_%A_Index% "`t" ; attach stored index
   t .= "`n"                    ; row is complete
}
MsgBox %t%                      ; show output
