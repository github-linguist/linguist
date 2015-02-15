For $i = 1 To 100
	If Mod($i, 15) = 0 Then
		MsgBox(0, "FizzBuzz", "FizzBuzz")
	ElseIf Mod($i, 5) = 0 Then
		MsgBox(0, "FizzBuzz", "Buzz")
	ElseIf Mod($i, 3) = 0 Then
		MsgBox(0, "FizzBuzz", "Fizz")
	Else
		MsgBox(0, "FizzBuzz", $i)
	EndIf
Next
