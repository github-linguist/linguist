#include <Array.au3>
$M = InputBox("Integer", "Enter biggest Integer")
Global $a[$M], $r[$M], $c = 1
For $i = 2 To $M -1
	If Not $a[$i] Then
		$r[$c] = $i
		$c += 1
		For $k = $i To $M -1 Step $i
			$a[$k] = True
		Next
	EndIf
Next
$r[0] = $c - 1
ReDim $r[$c]
_ArrayDisplay($r)
