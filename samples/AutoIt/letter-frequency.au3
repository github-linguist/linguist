Func _Letter_frequency($Path, $fcase = True, $fspecial_chars = True)
	Local $hFile, $sRead, $iupto, $iStart, $iCount
	If Not $fcase Then $fcase = False
	If Not $fspecial_chars Then
		$iStart = 64
		If Not $fcase Then
			$iupto = 26
		Else
			$iupto = 58
		EndIf
	Else
		$iStart = 31
		$iupto = 224
	EndIf
	$hFile = FileOpen($Path, 0)
	$sRead = FileRead($hFile)
	FileClose($hFile)
	For $i = 1 To $iupto
		If Not $fspecial_chars Then
			If $iStart + $i > 90 And $iStart + $i < 97 Then ContinueLoop
		EndIf
		$sRead = StringReplace($sRead, Chr($iStart + $i), "", 0, $fcase)
		$iCount = @extended
		If $iCount > 0 Then ConsoleWrite(Chr($iStart + $i) & " : " & $iCount & @CRLF)
	Next
EndFunc   ;==>_Letter_frequency
