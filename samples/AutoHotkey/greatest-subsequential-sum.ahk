seq = -1,-2,3,5,6,-2,-1,4,-4,2,-1
max := sum := start := 0
Loop Parse, seq, `,
   If (max < sum+=A_LoopField)
      max := sum, a := start, b := A_Index
   Else If sum <= 0
      sum := 0, start := A_Index
; read out the best subsequence
Loop Parse, seq, `,
   s .= A_Index > a && A_Index <= b ? A_LoopField "," : ""
MsgBox % "Max = " max "`n[" SubStr(s,1,-1) "]"
