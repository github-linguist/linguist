DllCall( "AllocConsole" ) ; create a console if not launched from one
hConsole := DllCall( "GetStdHandle", int, STDOUT := -11 )

MsgBox Resize the console...

VarSetCapacity(csbi, 22) ; CONSOLE_SCREEN_BUFFER_INFO structure
DllCall("GetConsoleScreenBufferInfo", UPtr, hConsole, UPtr, &csbi)
Left   := NumGet(csbi, 10, "short")
Top    := NumGet(csbi, 12, "short")
Right  := NumGet(csbi, 14, "short")
Bottom := NumGet(csbi, 16, "short")

columns	:= right - left + 1
rows	:= bottom - top + 1
MsgBox %columns% columns and %rows% rows
