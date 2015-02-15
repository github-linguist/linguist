MinNum = 1
MaxNum = 99999999999

Random, RandNum, %MinNum%, %MaxNum%
Loop
{
 InputBox, Guess, Number Guessing, Please enter a number between %MinNum% and %MaxNum%:,, 350, 130,,,,, %Guess%
 If ErrorLevel
  ExitApp
 If Guess Is Not Integer
 {
  MsgBox, 16, Error, Invalid guess.
  Continue
 }
 If Guess Not Between %MinNum% And %MaxNum%
 {
  MsgBox, 16, Error, Guess must be a number between %MinNum% and %MaxNum%.
  Continue
 }
 If A_Index = 1
  TotalTime = %A_TickCount%
 Tries = %A_Index%
 If Guess = %RandNum%
  Break
 If Guess < %RandNum%
  MsgBox, 64, Incorrect, The number guessed (%Guess%) was too low.
 If Guess > %RandNum%
  MsgBox, 64, Incorrect, The number guessed (%Guess%) was too high.
}
TotalTime := Round((A_TickCount - TotalTime) / 1000,1)
MsgBox, 64, Correct, The number %RandNum% was guessed in %Tries% tries, which took %TotalTime% seconds.
