ConsoleWrite(IntToBin(50) & @CRLF)

Func IntToBin($iInt)
	$Stack = ObjCreate("System.Collections.Stack")
	Local $b = -1, $r = ""
	While $iInt <> 0
		$b = Mod($iInt, 2)
		$iInt = INT($iInt/2)
		$Stack.Push ($b)
	WEnd
	For $i = 1 TO $Stack.Count
		$r &= $Stack.Pop
	Next
	Return $r
EndFunc   ;==>IntToBin
