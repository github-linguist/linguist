#include <Array.au3>
$Array = ZigZag(5)
_ArrayDisplay($Array)

Func ZigZag($int)
	Local $av_array[$int][$int]
	Local $x = 1, $y = 1
	For $I = 0 To $int ^ 2 -1
		$av_array[$x-1][$y-1] = $I
		If Mod(($x + $y), 2) = 0 Then ;Even
			if ($y < $int) Then
				$y += 1
			Else
				$x += 2
			EndIf
			if ($x > 1) Then $x -= 1
		Else ; ODD
			if ($x < $int) Then
				$x += 1
			Else
				$y += 2
			EndIf
			If $y > 1 Then $y -= 1
		EndIf
	Next
	Return $av_array
EndFunc   ;==>ZigZag
