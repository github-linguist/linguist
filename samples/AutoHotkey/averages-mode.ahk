MsgBox % Mode("1 2 3")
MsgBox % Mode("1 2 0 3 0.0")
MsgBox % Mode("0.1 2.2 -0.1 0.22e1 2.20 0.1")

Mode(a, d=" ") { ; the number that occurs most frequently in a list delimited by d (space)
   Sort a, ND%d%
   Loop Parse, a, %d%
      If (V != A_LoopField) {
         If (Ct > MxCt)
            MxV := V, MxCt := Ct
         V := A_LoopField, Ct := 1
      }
      Else Ct++
   Return Ct>MxCt ? V : MxV
}
