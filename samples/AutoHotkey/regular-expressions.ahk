MsgBox % foundpos := RegExMatch("Hello World", "World$")
MsgBox % replaced := RegExReplace("Hello World", "World$", "yourself")
