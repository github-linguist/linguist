lcs(a,b) { ; Longest Common Subsequence of strings, using Dynamic Programming
   Loop % StrLen(a)+2 {                          ; Initialize
      i := A_Index-1
      Loop % StrLen(b)+2
         j := A_Index-1, len%i%_%j% := 0
   }
   Loop Parse, a                                 ; scan a
   {
      i := A_Index, i1 := i+1, x := A_LoopField
      Loop Parse, b                              ; scan b
      {
         j := A_Index, j1 := j+1, y := A_LoopField
         len%i1%_%j1% := x=y ? len%i%_%j% + 1
         : (u:=len%i1%_%j%) > (v:=len%i%_%j1%) ? u : v
      }
   }
   x := StrLen(a)+1, y := StrLen(b)+1
   While x*y {                                   ; construct solution from lengths
     x1 := x-1, y1 := y-1
     If (len%x%_%y% = len%x1%_%y%)
         x := x1
     Else If  (len%x%_%y% = len%x%_%y1%)
         y := y1
     Else
         x := x1, y := y1, t := SubStr(a,x,1) t
   }
   Return t
}
