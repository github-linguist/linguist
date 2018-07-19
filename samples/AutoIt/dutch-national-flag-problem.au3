#include <Array.au3>
Dutch_Flag(50)
Func Dutch_Flag($arrayitems)
	Local $avArray[$arrayitems]
	For $i = 0 To UBound($avArray) - 1
		$avArray[$i] = Random(1, 3, 1)
	Next
	Local $low = 2, $high = 3, $i = 0
	Local $arraypos = -1
	Local $p = UBound($avArray) - 1
	While $i < $p
			if $avArray[$i] < $low Then
				$arraypos += 1
				_ArraySwap($avArray[$i], $avArray[$arraypos])
				$i += 1
			ElseIf $avArray[$i] >= $high Then
				_ArraySwap($avArray[$i], $avArray[$p])
				$p -= 1
			Else
				$i += 1
			EndIf
		WEnd
		_ArrayDisplay($avArray)
EndFunc   ;==>Dutch_Flag
