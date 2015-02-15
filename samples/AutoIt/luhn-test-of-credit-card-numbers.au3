Global $avarray[4] = [49927398716, 49927398717, 1234567812345678, 1234567812345670]
For $i = 0 To 3
	checkLuhn($avarray[$i])
Next

Func checkLuhn($number)
	$sum = 0
	$numDigits = StringSplit($number, "")
	For $i = $numDigits[0] - 1 To 1 Step -2
		$numDigits[$i] = $numDigits[$i] * 2
		If $numDigits[$i] >= 10 Then $numDigits[$i] -= 9
	Next
	For $i = 1 To $numDigits[0]
		$sum += $numDigits[$i]
	Next
	If StringRight($sum, 1) = "0" Then
		ConsoleWrite("Luhn-Check (" & $number & ") : True" & @CRLF)
		Return True
	Else
		ConsoleWrite("Luhn-Check (" & $number & ") : False" & @CRLF)
		Return False
	EndIf
EndFunc   ;==>checkLuhn
