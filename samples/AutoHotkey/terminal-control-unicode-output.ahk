DllCall("AllocConsole")
hConsole:=DllCall("GetConsoleWindow","UPtr")
Stdout:=FileOpen(DllCall("GetStdHandle", "int", -11, "ptr"), "h `n")
Stdin:=FileOpen(DllCall("GetStdHandle", "int", -10, "ptr"), "h `n")

;Full Unicode-support font needed
e:=SetConsoleOutputCP(65001)
if (e && A_IsUnicode)
{
	Print("△ - Unicode delta (U+25b3)")
	GetPos(x,y)
	if (x=0 && y=0) ;nothing prints if Non-Unicode font
		Print("Non-Unicode font")
}
else
	Print("Unicode not supported")
Pause()

Print(string=""){
	global Stdout
	if (!StrLen(string))
		return 1
	e:=DllCall("WriteConsole" . ((A_IsUnicode) ? "W" : "A")
			, "UPtr", Stdout.__Handle
			, "Str", string
			, "UInt", strlen(string)
			, "UInt*", Written
			, "uint", 0)
	if (!e) or (ErrorLevel)
		return 0 ;Failure
	Stdout.Read(0)
	return e
}

SetConsoleOutputCP(codepage) {
	e:=DllCall("SetConsoleOutputCP","UInt",codepage)
	if (!e) or (ErrorLevel)
		return 0 ;Failure
	return 1
}

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

Pause() {
	RunWait, %comspec% /c pause>NUL
}
