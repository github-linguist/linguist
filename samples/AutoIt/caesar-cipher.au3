$Caesar = Caesar("Hi", 2, True)
MsgBox(0, "Caesar", $Caesar)
Func Caesar($String, $int, $encrypt = True)
	If Not IsNumber($int) Or Not StringIsDigit($int) Then Return SetError(1, 0, 0)
	If $int < 1 Or $int > 25 Then Return SetError(2, 0, 0)
	Local $sLetters, $x
	$String = StringUpper($String)
	$split = StringSplit($String, "")
	For $i = 1 To $split[0]
		If Asc($split[$i]) - 64 > 26 Or Asc($split[$i]) - 64 < 1 Then
			$sLetters &= $split[$i]
			ContinueLoop
		EndIf
		If $encrypt = True Then
			$move = Asc($split[$i]) - 64 + $int
		Else
			$move = Asc($split[$i]) - 64 - $int
		EndIf
		If $move > 26 Then
			$move -= 26
		ElseIf $move < 1 Then
			$move += 26
		EndIf
		While $move
			$x = Mod($move, 26)
			If $x = 0 Then $x = 26
			$sLetters &= Chr($x + 64)
			$move = ($move - $x) / 26
		WEnd
	Next
	Return $sLetters
EndFunc   ;==>Caesar
