DllCall( "AllocConsole" ) ; create a console if not launched from one
hConsole := DllCall( "GetStdHandle", int, STDOUT := -11 )
Loop 10
{
	Loop 10
	{
		Random, asc, % asc("A"), % Asc("Z")
		WriteConsole(hConsole, Chr(asc))
	}
	WriteConsole(hConsole, "`n")
}

MsgBox % ReadConsoleOutputCharacter(hConsole, 1, 3, 6)

; === The below simply wraps part of the WinAPI ===

WriteConsole(hConsole, text){
	VarSetCapacity(out, 16)
	If DllCall( "WriteConsole", UPtr, hConsole, Str, text, UInt, StrLen(text)
				  , UPtrP, out, uint, 0 )
		return out
	return 0
}
ReadConsoleOutputCharacter(hConsole, length, x, y){
	VarSetCapacity(out, length * (1 << !!A_IsUnicode))
	VarSetCapacity(n, 16)
	if DllCall( "ReadConsoleOutputCharacter"
		, UPtr, hConsole
		, Str, out
		, UInt, length
		, UInt, x | (y << 16)
		, UPtrP, n )
			
		&& VarSetCapacity(out, -1)
				return out
	return 0
}
