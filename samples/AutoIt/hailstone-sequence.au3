$Hail = Hailstone(27)
ConsoleWrite("Sequence-Lenght: "&$Hail&@CRLF)
$Big = -1
$Sequenzlenght = -1
For $I = 1 To 100000
	$Hail = Hailstone($i, False)
	If Number($Hail) > $Sequenzlenght Then
	$Sequenzlenght = Number($Hail)
	$Big = $i
	EndIf
Next
ConsoleWrite("Longest Sequence : "&$Sequenzlenght&" from number "&$Big&@CRLF)
Func Hailstone($int, $sequence = True)
	$Counter = 0
	While True
		$Counter += 1
		If $sequence = True Then ConsoleWrite($int & ",")
		If $int = 1 Then ExitLoop
		If Not Mod($int, 2) Then
			$int = $int / 2
		Else
			$int = 3 * $int + 1
		EndIf
		If Not Mod($Counter, 25) AND $sequence = True Then ConsoleWrite(@CRLF)
	WEnd
	If $sequence = True Then ConsoleWrite(@CRLF)
	Return $Counter
EndFunc   ;==>Hailstone
