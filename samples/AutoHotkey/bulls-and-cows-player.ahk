length:=4, i:=0, S:=P(9,length)

Gui, Add, Text, w83 vInfo, Think of a %length%-digit number with no duplicate digits.
Gui, Add, Edit, w40 vBulls
Gui, Add, Text, x+3, Bulls
Gui, Add, Edit, xm w40 vCows
Gui, Add, Text, x+3, Cows
Gui, Add, Button, xm w83 Default vDefault, Start
Gui, Add, Edit, ym w130 r8 vHistory ReadOnly
Gui, Show
Return

ButtonStart:
	If Default = Restart
		Reload
	Gui, Submit, NoHide
	GuiControl, Focus, Bulls
	If (Bulls = length)
	{
		GuiControl, , Info, Guessed in %i% tries!
		GuiControl, , Default, Restart
		Default = Restart
	}
	Else
	{
		If i = 0
		{
			GuiControl, , Default, Submit
			GuiControl, , History
		}
		Else
		{
			If (StrLen(Bulls) != 1 || StrLen(Cows) != 1)
				Return
			If Bulls is not digit
				Return
			If Cows is not digit
				Return
			GuiControl, , History, % History .= ": " Bulls " Bulls " Cows " Cows`n"
			GuiControl, , Bulls
			GuiControl, , Cows
			
			S:=Remove(S, Guess, Bulls, Cows)
		}
		If !S
		{
			GuiControl, , Info, Invalid response.
			GuiControl, , Default, Restart
			Default = Restart
		}
		Else
		{
			Guess := SubStr(S,1,length)
			GuiControl, , History, % History . Guess
			GuiControl, , Info, Enter a single digit number of bulls and cows.
			i++
		}
	}
Return

GuiEscape:
GuiClose:
	ExitApp

Remove(S, Guess, Bulls, Cows) {
	Loop, Parse, S, `n
		If (Bulls "," Cows = Response(Guess, A_LoopField))
			S2 .= A_LoopField . "`n"
	Return SubStr(S2,1,-1)
}

; from http://rosettacode.org/wiki/Bulls and Cows#AutoHotkey
Response(Guess,Code) {
	Bulls := 0, Cows := 0
	Loop, % StrLen(Code)
		If (SubStr(Guess, A_Index, 1) = SubStr(Code, A_Index, 1))
			Bulls++
		Else If (InStr(Code, SubStr(Guess, A_Index, 1)))
			Cows++
	Return Bulls "," Cows
}

; from http://rosettacode.org/wiki/Permutations#Alternate_Version
P(n,k="",opt=0,delim="",str="") {
	i:=0
	If !InStr(n,"`n")
		If n in 2,3,4,5,6,7,8,9
			Loop, %n%
				n := A_Index = 1 ? A_Index : n "`n" A_Index
		Else
			Loop, Parse, n, %delim%
				n := A_Index = 1 ? A_LoopField : n "`n" A_LoopField
	If (k = "")
		RegExReplace(n,"`n","",k), k++
	If k is not Digit
		Return "k must be a digit."
	If opt not in 0,1,2,3
		Return "opt invalid."
	If k = 0
		Return str
	Else
		Loop, Parse, n, `n
			If (!InStr(str,A_LoopField) || opt & 1)
				s .= (!i++ ? (opt & 2 ? str "`n" : "") : "`n" )
					. P(n,k-1,opt,delim,str . A_LoopField . delim)
		Return s
}
