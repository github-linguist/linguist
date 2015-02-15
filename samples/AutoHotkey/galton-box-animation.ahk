AutoTrim Off
; User settings
bottompegs := 6
SleepTime  := 200
fallspace  := 30

; create the board
out := (pad2 := Space(bottompegs*2+1)) "`n"
Loop % bottompegs
{
	out .= Space(bottompegs-A_Index+1)
	Loop % A_Index
		out .= "* "
	out .= Space(bottompegs-A_Index+1) . "`n"
}
StringTrimRight, strboard, out, 1 ; remove last newline
Loop % fallspace-1
	strboard .= "`n" . pad2
strboard .= "`n"
Loop % bottompegs*2+1
	strboard .= "="

; Create Gui
Gui Font, , Consolas
Gui -Caption
Gui Margin, 0, 0
Gui, Add, edit, -VScroll vE, % strboard
Gui Show
Loop
{
	ballX := bottompegs+1, BallY := 1
	strboard := ChangeChar(strboard, BallX, ballY, "O")
	GuiControl,, E, % strboard
	sleep SleepTime
	; Make ball fall and bounce
	Loop % bottompegs
	{
		strboard := ChangeChar(strboard, BallX, BallY, " ")
		ballY += 1
		ballX += RandAdd()
		; MsgBox % ballX ", " ballY
		GuiControl,, E, % strboard := ChangeChar(strboard, ballX, ballY, "O")
		sleep SleepTime
	}
	; now fall to the bottom
	While GetChar(strboard, BallX, BallY+1) = A_Space
	{
		strboard := ChangeChar(strboard, BallX, BallY, " ")
		BallY += 1
		strboard := ChangeChar(strboard, BallX, BallY, "O")
		GuiControl,, E, % strboard
		sleep SleepTime
	}
}
~Esc::
GuiClose:
ExitApp

Space(n){
	If n
		return " " Space(n-1)
	return ""
}
RandAdd(){
	Random, n, 3, 4
	return (n=3 ? -1 : 1)
}

GetChar(s, x, y){
	Loop Parse, s, `n
		if (A_Index = y)
			return SubStr(A_LoopField, x, 1)
}
ChangeChar(s, x, y, c){
	Loop Parse, s, `n
	{
		If (A_Index = y)
		{
			Loop Parse, A_LoopField
				If (A_Index = x)
					out .= c
				else    out .= A_LoopField
		}
		else out .= A_LoopField
		     out .= "`n"
	}
	StringTrimRight, out, out, 1 ; removes the last newline
	return out
}
