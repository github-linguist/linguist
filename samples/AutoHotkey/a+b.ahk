InputBox, input , A+B, Two integer numbers`, separated by space.
StringSplit, output, input, %A_Space%
msgbox, % output1 . "+" . output2 "=" output1+output2
