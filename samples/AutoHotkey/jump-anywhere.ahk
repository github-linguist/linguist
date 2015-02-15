; Define a function.
function()
{
	MsgBox, Start
	gosub jump
	
	free:
	MsgBox, Success
}

; Call the function.
function()
goto next
return

jump:
MsgBox, Suspended
return

next:
Loop, 3
{
	gosub jump
}
return

/*
Output (in Message Box):

Start
Suspended
Success
Suspended
Suspended
Suspended

*/
