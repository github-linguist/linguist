DllCall("AllocConsole")
Octal(int){
	While int
		out := Mod(int, 8) . out, int := int//8
	return out
}
Loop
{
	FileAppend, % Octal(A_Index) "`n", CONOUT$
	Sleep 200
}
