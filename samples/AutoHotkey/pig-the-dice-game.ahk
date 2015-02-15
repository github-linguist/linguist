Gui, Font, s12, Verdana
Gui, Add, Text, vPlayer0, Player 0
Gui, Add, Text, vSum0, 000
Gui, Add, Button, Default, Roll
Gui, Add, Text, ys vLastRoll, Roll 0
Gui, Add, Text, vTurnSum, Sum 000
Gui, Add, Button, , Hold
Gui, Add, Text, ys vPlayer1, Player 1
Gui, Add, Text, vSum1, 000
Gui, Add, Button, , Reload
Gui, Show
GuiControl, Disable, Player1

CurrentPlayer := 0
ButtonRoll:
Loop 10
{
	Random, LastRoll, 1, 6
	GuiControl, , LastRoll, Roll %LastRoll%
	Sleep 50
}
If LastRoll != 1
{
	TurnSum += LastRoll
	GuiControl, , TurnSum, Sum %TurnSum%
	Return
}
TurnSum := 0
ButtonHold:
Sum%CurrentPlayer% += TurnSum
TurnSum := 0
GuiControl, , LastRoll, Roll
GuiControl, , TurnSum, Sum %TurnSum%
GuiControl, , Sum%CurrentPlayer%, % Sum%CurrentPlayer%
If Sum%CurrentPlayer% >= 100
{
	MsgBox Player %CurrentPlayer% Won!
	GuiClose:
	ExitApp
}
GuiControl, Disable, Player%CurrentPlayer%
CurrentPlayer := !CurrentPlayer
GuiControl, Enable, Player%CurrentPlayer%
Return

ButtonReload:
Reload
