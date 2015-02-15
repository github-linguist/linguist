; Submitted by MasterFocus --- http://tiny.cc/iTunis

ScrambledList := CorrectList := "1 2 3 4 5 6 7 8 9" ; Declare two identical correct sequences
While ( ScrambledList = CorrectList )
  Sort, ScrambledList, Random D%A_Space% ; Shuffle one of them inside a While-loop to ensure it's shuffled

Attempts := 0
While ( ScrambledList <> CorrectList ) ; Repeat until the sequence is correct
{
  InputBox, EnteredNumber, Number Reversal Game, Attempts so far: %Attempts%`n`nCurrent sequence: %ScrambledList%`n`nHow many numbers (from the left) should be flipped?, , 400, 200
  If ErrorLevel
    ExitApp ; Exit if user presses ESC or Cancel
  If EnteredNumber is not integer
    Continue ; Discard attempt if entered number is not an integer
  If ( EnteredNumber <= 1 )
    Continue ; Discard attempt if entered number is <= 1
  Attempts++ ; Increase the number of attempts
  ; Reverse the first part of the string and add the second part
  ; The entered number is multiplied by 2 to deal with the spaces
  ScrambledList := Reverse(SubStr(ScrambledList,1,(EnteredNumber*2)-1)) SubStr(ScrambledList,EnteredNumber*2)
}

MsgBox, You took %Attempts% attempts to get the correct sequence. ; Final message

Return

;-------------------

Reverse(Str) ; Auxiliar function (flips a string)
{
  Loop, Parse, Str
    Out := A_LoopField Out
  Return Out
}
