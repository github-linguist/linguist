MsgBox % isDir_empty(A_ScriptDir)?"true":"false"

isDir_empty(p) {
	Loop, %p%\* , 1
		return 0
	return 1
}
