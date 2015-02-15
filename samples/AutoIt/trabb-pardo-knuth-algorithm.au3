; Trabb Pardo–Knuth algorithm
; by James1337 (autoit.de)
; AutoIt Version: 3.3.8.1

Local $S, $i, $y

Do
	$S = InputBox("Trabb Pardo–Knuth algorithm", "Please enter 11 numbers:", "1 2 3 4 5 6 7 8 9 10 11")
	If @error Then Exit
	$S = StringSplit($S, " ")
Until ($S[0] = 11)

For $i = 11 To 1 Step -1
	$y = f($S[$i])
	If ($y > 400) Then
		ConsoleWrite("f(" & $S[$i] & ") = Overflow!" & @CRLF)
	Else
		ConsoleWrite("f(" & $S[$i] & ") = " & $y & @CRLF)
	EndIf
Next

Func f($x)
	Return Sqrt(Abs($x)) + 5*$x^3
EndFunc
