FileSelectFile, filename, , %A_ScriptDir%, Select a Mad Libs template, *.txt
If ErrorLevel
	ExitApp ; the user canceled the file selection
FileRead, contents, %filename%
pos := match := 0
While pos := RegExMatch(contents, "<[^>]+>", match, pos+strLen(match))
{
	InputBox, repl, Mad Libs, Enter a replacement for %match%:
	If ErrorLevel ; cancelled inputbox
		ExitApp
	StringReplace, contents, contents, %match%, %repl%, All
}
MsgBox % contents
