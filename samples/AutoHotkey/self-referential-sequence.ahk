; The following directives and commands speed up execution:
#NoEnv
SetBatchlines -1
ListLines Off
Process, Priority,, high

iterations := 0, seed := "Seeds: "

Loop 1000000
	If (newIterations := CountSubString(list := ListSequence(A_Index), "`n")) > iterations
		iterations := newiterations
		,final := "`nIterations: " iterations+1 "`nSequence:`n`n" A_Index "`n" list
		,seed := A_Index " "
	else if (newIterations = iterations)
		seed .= A_Index " "
MsgBox % "Seeds: " . seed . final
ListSequence(seed){
	While !InStr("`n" . out, "`n" (d:=Describe(seed)) "`n")
		out .= d "`n", seed := d
	return out
}

Describe(n){
	Loop 10
		If (t:=CountSubString(n, 10-A_Index))
			out .= t . (10-A_Index)
	return out
}

CountSubstring(fullstring, substring){
   StringReplace, junk, fullstring, %substring%, , UseErrorLevel
   return errorlevel
}
