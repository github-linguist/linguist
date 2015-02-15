DllCall("AllocConsole") ; Create a console if not launched from one
hConsole := DllCall("GetStdHandle", UInt, STDOUT := -11)

VarSetCapacity(cci, 8)  ; CONSOLE_CURSOR_INFO structure
DllCall("GetConsoleCursorInfo", UPtr, hConsole, UPtr, &cci)
NumPut(0, cci, 4)
DllCall("SetConsoleCursorInfo", UPtr, hConsole, UPtr, &cci)

FileAppend, Cursor hidden for 3 seconds..., CONOUT$ ; Prints to stdout
Sleep 3000

NumPut(1, cci, 4)
DllCall("SetConsoleCursorInfo", UPtr, hConsole, UPtr, &cci)

FileAppend, `nCursor shown, CONOUT$
MsgBox
