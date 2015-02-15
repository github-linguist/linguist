DllCall( "AllocConsole" ) ; create a console if not launched from one
hConsole := DllCall( "GetStdHandle", int, STDOUT := -11 )

DllCall("SetConsoleCursorPosition", UPtr, hConsole, UInt, (6 << 16) | 3)
WriteConsole(hConsole, "Hello")

MsgBox

WriteConsole(hConsole, text){
	VarSetCapacity(out, 16)
	If DllCall( "WriteConsole", UPtr, hConsole, Str, text, UInt, StrLen(text)
				  , UPtrP, out, uint, 0 )
		return out
	return 0
}
