MaxGuesses = 50

GetParams(LowerBound,UpperBound)
If Not GuessNum(LowerBound,UpperBound,MaxGuesses)
 MsgBox, 16, Error, Could not guess number within %MaxGuesses% guesses.

GetParams(ByRef LowerBound,ByRef UpperBound)
{
 WinWait, Number Guessing ahk_class #32770
 Sleep, 100
 WinGet, InputID, ID
 ControlGetText, Temp1, Static1, ahk_id %InputID%
 Temp2 := InStr(Temp1,A_Space,False,32)
 LowerBound := SubStr(Temp1,31,Temp2 - 31)
 UpperBound := SubStr(Temp1,Temp2 + 5,-1)
}

GuessNum(LowerBound,UpperBound,MaxGuesses)
{
 Loop, %MaxGuesses%
 {
  Guess := LowerBound + ((UpperBound - LowerBound) // 2)
  Temp1 := SendGuess(Guess)
  ToolTip % Temp1
  If Temp1 = Too Low
   LowerBound = %Guess%
  Else If Temp1 = Too High
   UpperBound = %Guess%
  Else
   Return, 1
 }
}

SendGuess(Guess)
{
 WinGet, InputID, ID, Number Guessing ahk_class #32770
 ControlSetText, Edit1, %Guess%, ahk_id %InputID%
 ControlSend, Button1, {Enter}, ahk_id %InputID%
 Loop
 {
  Sleep, 50
  IfWinExist, Correct ahk_class #32770
   Return
  Else IfWinExist, Incorrect ahk_class #32770
   Break
 }
 ControlGetText, Temp1, Static2
 WinClose
 WinWaitClose
 IfInString, Temp1, low
  Return, "Too Low"
 Else
  Return, "Too High"
}
