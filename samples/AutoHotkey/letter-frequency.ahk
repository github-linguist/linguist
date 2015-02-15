OpenFile = %A_ScriptFullPath% ; use own source code
FileRead, FileText, %OpenFile%
Loop 26
{
	StringReplace, junk, FileText, % Chr(96+A_Index),, UseErrorLevel
	out .= Chr(96+A_Index) ": " ErrorLevel "`n"
}
MsgBox % out
