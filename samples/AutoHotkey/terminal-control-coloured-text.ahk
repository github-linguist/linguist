DllCall( "AllocConsole" ) ; create a console if not launched from one
hConsole := DllCall( "GetStdHandle", int, STDOUT := -11 )
Loop 15
	 SetConsoleTextAttribute(hConsole, A_Index)
	,WriteConsole(hConsole, "AutoHotkey`n")

MsgBox

SetConsoleTextAttribute(hConsole, Attributes){
	return DllCall( "SetConsoleTextAttribute", UPtr, hConsole, UShort, Attributes)
}
WriteConsole(hConsole, text){
	VarSetCapacity(out, 16)
	If DllCall( "WriteConsole", UPtr, hConsole, Str, text, UInt, StrLen(text)
				  , UPtrP, out, uint, 0 )
		return out
	return 0
}
