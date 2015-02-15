GUI, add, Edit,Number w50 vUserInput, 0          ; Number Specifies Numbers-only, but other characters can still be pasted in,
						 ; Making our own check necessary. (MakeSure)

GUI, add, Button, gIncrement, Increment 	 ; Instead of an increment button, the UpDown control could be used, but this was not specified.
GUI, add, Button, gRando, Random		
Gui, Show, W200 y200, Title			 ; Shows the GUI with a width and height of 200px
SetTimer, MakeSure, 1000 			 ; Runs MakeSure every second
return 						 ; End Auto-Execute Section


Increment:
Gui, Submit, NoHide
; The above line assigns all variables associated with controls to the state of that control, but leaves the GUI visible.
If UserInput is not Number
{
	MsgBox, %userInput% is not a number.
	GUIControl,,UserInput, 0 ; Reset the Edit control to 0
}
Else
{
	UserInput++
	GUIControl,, UserInput, %UserInput% ; Sets the value of the Edit control
}
return



Rando:
MsgBox, 4, Title, Are you sure you want to randomize? ; Specify your own title. 4 means YesNo
IfMsgBox, Yes
{
	Random, UserInput, 1, 999		      ; random number from 1-999
	GUIControl,, UserInput, %UserInput%	      ; Sets the value of the Edit control
}
return



MakeSure:
Gui, Submit, NoHide
If UserInput is not Number
{
	If (UserInput<>"")
	{
	Msgbox Error! Numbers Only!
	GUIControl,, UserInput, 0
	}
}
return



GUIClose:
ExitApp  ; Makes sure the script exits when the window is closed,
	 ; Otherwise the script is persistent because it contains
	 ; a timer.
