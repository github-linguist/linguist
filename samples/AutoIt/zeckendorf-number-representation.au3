For $i = 0 To 20
	ConsoleWrite($i &": "& Zeckendorf($i)&@CRLF)
Next

Func Zeckendorf($int, $Fibarray = "")
	If Not IsArray($Fibarray) Then $Fibarray = Fibonacci($int)
	Local $ret = ""
	For $i = UBound($Fibarray) - 1 To 1 Step -1
		If $Fibarray[$i] > $int And $ret = "" Then ContinueLoop ; dont use Leading  Zeros
		If $Fibarray[$i] > $int Then
			$ret &= "0"
		Else
			If StringRight($ret, 1) <>  "1" Then
				$ret &= "1"
				$int -= $Fibarray[$i]
			Else
				$ret &= "0"
			EndIf
		EndIf
	Next
	If $ret = "" Then $ret = "0"
	Return $ret
EndFunc   ;==>Zeckendorf

Func Fibonacci($max)
	$AList = ObjCreate("System.Collections.ArrayList")
	$AList.add("0")
	$current = 0
	While True
		If $current > 1 Then
			$count = $AList.Count
			$current = $AList.Item($count - 1)
			$current = $current + $AList.Item($count - 2)
		Else
			$current += 1
		EndIf
		$AList.add($current)
		If $current > $max Then ExitLoop
	WEnd
	$Array = $AList.ToArray
	Return $Array
EndFunc   ;==>Fibonacci
