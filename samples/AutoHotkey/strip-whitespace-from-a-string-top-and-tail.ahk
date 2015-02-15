string := "   abc   "
MsgBox % clipboard := "<" LTrim(string) ">`n<" RTrim(string) ">`n<" . Trim(string) ">"
