Array := []
InputBox, data,, Enter two integers separated by a Space:`n(ex. 5 7)
StringSplit, i, data, %A_Space%
Array[i1,i2] := "that element"
MsgBox, % "Array[" i1 "," i2 "] = " Array[i1,i2]
