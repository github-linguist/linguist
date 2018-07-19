array = 1,2,3,4,5,6,7
loop, parse, array, `,
{
    if IsEven(A_LoopField)
        evens = %evens%,%A_LoopField%
}
stringtrimleft, evens, evens, 1
msgbox % evens
return

IsEven(number)
{
    return !mod(number, 2)
}


; ----- Another version: always with csv string ------
array = 1,2,3,4,5,6,7

even(s) {
	loop, parse, s, `,
		if !mod(A_LoopField, 2)
			r .= "," A_LoopField
	return SubStr(r, 2)
}

MsgBox % "Array => " array "`n" "Result => " even(array)


; ----- Yet another version: with array (requires AutoHotKey_L) ------
array2 := [1,2,3,4,5,6,7]

even2(a) {
	r := []
	For k, v in a
		if !mod(v, 2)
			r.Insert(v)
	return r
}

; Add "join" method to string object (just like python)
s_join(o, a) {
	Loop, % a.MaxIndex()
		r .= o a[A_Index]
	return SubStr(r, StrLen(o) + 1)
}
"".base.join := Func("s_join")

MsgBox % "Array => " ",".join(array2) "`n"  "Result => " ",".join(even2(array2))
