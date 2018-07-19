#include <Array.au3>
Local $Array[1]
_ArrayAdd($Array, "[]")
_ArrayAdd($Array, "[][]")
_ArrayAdd($Array, "[[][]]")
_ArrayAdd($Array, "][")
_ArrayAdd($Array, "][][")
_ArrayAdd($Array, "[]][[]")

For $i = 0 To UBound($Array) -1
	Balanced_Brackets($Array[$i])
	If @error Then
		ConsoleWrite($Array[$i] &" = NOT OK"&@CRLF)
	Else
		ConsoleWrite($Array[$i] &" = OK"&@CRLF)
	EndIf
Next

Func Balanced_Brackets($String)
	Local $cnt = 0
	$Split = Stringsplit($String, "")
	For $i = 1 To $Split[0]
		If $split[$i] = "[" Then $cnt += 1
		If $split[$i] = "]" Then $cnt -= 1
		If $cnt < 0 Then Return SetError(1,0,0)
	Next
	Return 1
EndFunc
