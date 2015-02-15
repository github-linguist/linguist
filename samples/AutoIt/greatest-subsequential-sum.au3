Local $iArray[11] = [-1, -2, 3, 5, 6, -2, -1, 4, -4, 2, -1]
GREAT_SUB($iArray)
Local $iArray[5] = [-1, -2, -3, -4, -5]
GREAT_SUB($iArray)
Local $iArray[15] = [7, -6, -8, 5, -2, -6, 7, 4, 8, -9, -3, 2, 6, -4, -6]
GREAT_SUB($iArray)

Func GREAT_SUB($iArray)
	Local $iSUM = 0, $iBEGIN_MAX = 0, $iEND_MAX = -1, $iMAX_SUM = 0
	For $i = 0 To UBound($iArray) - 1
		$iSUM = 0
		For $k = $i To UBound($iArray) - 1
			$iSUM += $iArray[$k]
			If $iSUM > $iMAX_SUM Then
				$iMAX_SUM = $iSUM
				$iEND_MAX = $k
				$iBEGIN_MAX = $i
			EndIf
		Next
	Next
	ConsoleWrite("> Array: [")
	For $i = 0 To UBound($iArray) - 1
		If $iArray[$i] > 0 Then ConsoleWrite("+")
		ConsoleWrite($iArray[$i])
		If $i <> UBound($iArray) - 1 Then ConsoleWrite(",")
	Next
	ConsoleWrite("]" & @CRLF & "+>Maximal subsequence: [")
	$iSUM = 0
	For $i = $iBEGIN_MAX To $iEND_MAX
		$iSUM += $iArray[$i]
		If $iArray[$i] > 0 Then ConsoleWrite("+")
		ConsoleWrite($iArray[$i])
		If $i <> $iEND_MAX Then ConsoleWrite(",")
	Next
	ConsoleWrite("]" & @CRLF & "!>SUM of subsequence: " & $iSUM & @CRLF)
EndFunc   ;==>GREAT_SUB
