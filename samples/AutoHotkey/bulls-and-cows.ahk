length:=4, Code:="" ; settings

While StrLen(Code) < length {
	Random, num, 1, 9
	If !InStr(Code, num)
		Code .= num
}
Gui, Add, Text, w83 vInfo, I'm thinking of a %length%-digit number with no duplicate digits.
Gui, Add, Edit, wp vGuess, Enter a guess...
Gui, Add, Button, wp Default vDefault, Submit
Gui, Add, Edit, ym w130 r8 vHistory ReadOnly
Gui, Show
Return

ButtonSubmit:
	If Default = Restart
		Reload
	Gui, Submit, NoHide
	If (StrLen(Guess) != length)
		GuiControl, , Info, Enter a %length%-digit number.
	Else If Guess is not digit
		GuiControl, , Info, Enter a %length%-digit number.
	Else
	{
		GuiControl, , Info
		GuiControl, , Guess
		If (Guess = Code)
		{
			GuiControl, , Info, Correct!
			GuiControl, , Default, Restart
			Default = Restart
		}
		response := Response(Guess, Code)
		Bulls := SubStr(response, 1, InStr(response,",")-1)
		Cows := SubStr(response, InStr(response,",")+1)
		GuiControl, , History, % History . Guess ": " Bulls " Bulls " Cows " Cows`n"
	}
Return

GuiEscape:
GuiClose:
	ExitApp

Response(Guess,Code) {
	Bulls := 0, Cows := 0
	Loop, % StrLen(Code)
		If (SubStr(Guess, A_Index, 1) = SubStr(Code, A_Index, 1))
			Bulls++
		Else If (InStr(Code, SubStr(Guess, A_Index, 1)))
			Cows++
	Return Bulls "," Cows
}
