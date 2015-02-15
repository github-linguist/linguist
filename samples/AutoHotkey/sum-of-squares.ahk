list = 3 1 4 1 5 9
Loop, Parse, list, %A_Space%
 sum += A_LoopField**2
MsgBox,% sum
