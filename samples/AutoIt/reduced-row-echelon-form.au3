Global $ivMatrix[3][4] = [[1, 2, -1, -4],[2, 3, -1, -11],[-2, 0, -3, 22]]
ToReducedRowEchelonForm($ivMatrix)

Func ToReducedRowEchelonForm($matrix)
	Local $clonematrix, $i
	Local $lead = 0
	Local $rowCount = UBound($matrix) - 1
	Local $columnCount = UBound($matrix, 2) - 1
	For $r = 0 To $rowCount
		If $columnCount = $lead Then ExitLoop
		$i = $r
		While $matrix[$i][$lead] = 0
			$i += 1
			If $rowCount = $i Then
				$i = $r
				$lead += 1
				If $columnCount = $lead Then ExitLoop
			EndIf
		WEnd
		; There´s no built in Function to swap Rows of a 2-Dimensional Array
		; We need to clone our matrix to swap complete lines
		$clonematrix = $matrix ; Swap Lines, no
		For $s = 0 To $columnCount
			$matrix[$r][$s] = $clonematrix[$i][$s]
			$matrix[$i][$s] = $clonematrix[$r][$s]
		Next
		Local $m = $matrix[$r][$lead]
		For $k = 0 To $columnCount
			$matrix[$r][$k] = $matrix[$r][$k] / $m
		Next
		For $i = 0 To $rowCount
			If $i <> $r Then
				Local $m = $matrix[$i][$lead]
				For $k = 0 To $columnCount
					$matrix[$i][$k] -= $m * $matrix[$r][$k]
				Next
			EndIf
		Next
		$lead += 1
	Next
	; Console Output
	For $i = 0 To $rowCount
		ConsoleWrite("[")
		For $k = 0 To $columnCount
			ConsoleWrite($matrix[$i][$k])
			If $k <> $columnCount Then ConsoleWrite(",")
		Next
		ConsoleWrite("]" & @CRLF)
	Next
	; End of Console Output
	Return $matrix
EndFunc   ;==>ToReducedRowEchelonForm
