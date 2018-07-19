MsgBox % diff("2,3,4,3",1)
MsgBox % diff("2,3,4,3",2)
MsgBox % diff("2,3,4,3",3)
MsgBox % diff("2,3,4,3",4)

diff(list,ord) { ; high order forward differences of a list
   Loop %ord% {
      L =
      Loop Parse, list, `, %A_Space%%A_Tab%
         If (A_Index=1)
            p := A_LoopField
         Else
            L .= "," A_LoopField-p, p := A_LoopField
      list := SubStr(L,2)
   }
   Return list
}
