DllCall( "AllocConsole" ) ; create a console if not launched from one
hConsole := DllCall( "GetStdHandle", int, STDOUT := -11 )

SetConsoleTextAttribute(hConsole, 0x70) ; gray background, black foreground
FileAppend, Reversed`n, CONOUT$ ; print to stdout

SetConsoleTextAttribute(hConsole, 0x07) ; black background, gray foreground
FileAppend, Normal, CONOUT$

MsgBox

SetConsoleTextAttribute(hConsole, Attributes){
	return DllCall( "SetConsoleTextAttribute", UPtr, hConsole, UShort, Attributes)
}
