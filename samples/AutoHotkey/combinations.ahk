MsgBox % Comb(1,1)
MsgBox % Comb(3,3)
MsgBox % Comb(3,2)
MsgBox % Comb(2,3)
MsgBox % Comb(5,3)

Comb(n,t) { ; Generate all n choose t combinations of 1..n, lexicographically
   IfLess n,%t%, Return
   Loop %t%
      c%A_Index% := A_Index
   i := t+1, c%i% := n+1

   Loop {
      Loop %t%
         i := t+1-A_Index, c .= c%i% " "
      c .= "`n"     ; combinations in new lines
      j := 1, i := 2
      Loop
         If (c%j%+1 = c%i%)
             c%j% := j, ++j, ++i
         Else Break
      If (j > t)
         Return c
      c%j% += 1
   }
}
