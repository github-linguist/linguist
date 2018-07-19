Dim $a[10]
ConsoleWrite('array before permutation:' & @CRLF)
For $i = 0 To 9
	$a[$i] = Random(20,100,1)
	ConsoleWrite($a[$i] & ' ')
Next
ConsoleWrite(@CRLF)

_Permute($a)
ConsoleWrite('array after permutation:' & @CRLF)
For $i = 0 To UBound($a) -1
	ConsoleWrite($a[$i] & ' ')
Next
ConsoleWrite(@CRLF)


Func _Permute(ByRef $array)
	Local $random, $tmp
	For $i = UBound($array) -1 To 0 Step -1
		$random = Random(0,$i,1)
		$tmp = $array[$random]
		$array[$random] = $array[$i]
		$array[$i] = $tmp
	Next
EndFunc
