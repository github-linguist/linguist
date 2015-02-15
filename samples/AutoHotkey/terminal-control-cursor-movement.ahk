DllCall("AllocConsole")
hConsole:=DllCall("GetConsoleWindow","UPtr")
Stdout:=FileOpen(DllCall("GetStdHandle", "int", -11, "ptr"), "h `n")
Stdin:=FileOpen(DllCall("GetStdHandle", "int", -10, "ptr"), "h `n")

;move the cursor one position to the left
GetPos(x,y)
SetPos(x-1)

;move the cursor one position to the right
GetPos(x,y)
SetPos(x+1)

;move the cursor up one line (without affecting its horizontal position)
GetPos(x,y)
SetPos(x,y-1)

;move the cursor down one line (without affecting its horizontal position)
GetPos(x,y)
SetPos(x,y+1)

;move the cursor to the beginning of the line
GetPos(x,y)
SetPos(0,y)

;move the cursor to the end of the line
;requires previous knowledge of screen width -- typically 80
SetPos(79) ;minus 1 because origin is (0,0)

;move the cursor to the top left corner of the screen
SetPos(0,0)

;move the cursor to the bottom right corner of the screen
GetConsoleSize(w,h)
SetPos(w-1,h-1) ;minus 1 because origin is (0,0)

GetPos(ByRef x, ByRef y) {
	global Stdout
	VarSetCapacity(struct,22,0)
	e:=DllCall("GetConsoleScreenBufferInfo","UPtr",Stdout.__Handle,"Ptr",&struct)
	if (!e) or (ErrorLevel)
			return 0 ;Failure
	x:=NumGet(&struct,4,"UShort")
	y:=NumGet(&struct,6,"UShort")
	return 1
}

SetPos(x="",y="") {
	global Stdout
	GetPos(ox,oy)
	if x is not Integer
			x:=ox
	if y is not Integer
			y:=oy
	VarSetCapacity(struct,4,0)
	Numput(x,struct,"UShort")
	Numput(y,struct,2,"UShort")
	e:=DllCall("SetConsoleCursorPosition","Ptr",Stdout.__Handle,"uint",Numget(struct,"uint"))
	if (!e) or (ErrorLevel)
			return 0 ;Failure
	return 1
}

GetConsoleSize(ByRef bufferwidth, ByRef bufferheight) {
	global Stdout
	VarSetCapacity(struct,22,0)
	x:=DllCall("GetConsoleScreenBufferInfo","UPtr",Stdout.__Handle,"Ptr",&struct)
	if (!x) or (ErrorLevel)
		return 0 ;Failure
	bufferwidth:=NumGet(&struct,"UShort")
	bufferheight:=NumGet(&struct,2,"UShort")
	return 1
}
