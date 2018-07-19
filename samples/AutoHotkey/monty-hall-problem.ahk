#NoTrayIcon
#SingleInstance, OFF
#Persistent
SetBatchLines, -1
Iterations = 1000
Loop, %Iterations%
{
   If Monty_Hall(1)
      Correct_Change++
   Else
      Incorrect_Change++
   If Monty_Hall(2)
      Correct_Random++
   Else
      Incorrect_Random++
   If Monty_Hall(3)
      Correct_Stay++
   Else
      Incorrect_Stay++
}
Percent_Change := floor(Correct_Change / Iterations * 100)
Percent_Random := floor(Correct_Random / Iterations * 100)
Percent_Stay := floor(Correct_Stay / Iterations * 100)
MsgBox,, Monty Hall Problem, These are the results:`r`n`r`nWhen I changed my guess, I got %Correct_Change% of %Iterations% (that's %Incorrect_Change% incorrect). Thats %Percent_Change%`% correct.`r`nWhen I randomly changed my guess, I got %Correct_Random% of %Iterations% (that's %Incorrect_Random% incorrect). Thats %Percent_Random%`% correct.`r`nWhen I stayed with my first guess, I got %Correct_Stay% of %Iterations% (that's %Incorrect_Stay% incorrect). Thats %Percent_Stay%`% correct.
ExitApp
Monty_Hall(Mode) ;Mode is 1 for change, 2 for random, or 3 for stay
{
   Random, prize, 1, 3
   Random, guess, 1, 3
   If (prize = guess && Mode != 3)
      While show != 0 && show != guess
         Random, show, 1, 3
   Else
      show := 6 - prize - guess
   Random, change_guess, 0, 1
   If (Mode = 1 || (change_guess && Mode = 2))
      Return, (6 - show - guess) = prize
   Else If (Mode = 3 || (!change_guess && Mode = 2))
      Return, guess = prize
   Else
      Return
}
