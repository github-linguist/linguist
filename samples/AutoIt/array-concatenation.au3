_ArrayConcatenate($avArray, $avArray2)
Func _ArrayConcatenate(ByRef $avArrayTarget, Const ByRef $avArraySource, $iStart = 0)
	If Not IsArray($avArrayTarget) Then Return SetError(1, 0, 0)
	If Not IsArray($avArraySource) Then Return SetError(2, 0, 0)
	If UBound($avArrayTarget, 0) <> 1 Then
		If UBound($avArraySource, 0) <> 1 Then Return SetError(5, 0, 0)
		Return SetError(3, 0, 0)
	EndIf
	If UBound($avArraySource, 0) <> 1 Then Return SetError(4, 0, 0)

	Local $iUBoundTarget = UBound($avArrayTarget) - $iStart, $iUBoundSource = UBound($avArraySource)
	ReDim $avArrayTarget[$iUBoundTarget + $iUBoundSource]
	For $i = $iStart To $iUBoundSource - 1
		$avArrayTarget[$iUBoundTarget + $i] = $avArraySource[$i]
	Next

	Return $iUBoundTarget + $iUBoundSource
EndFunc   ;==>_ArrayConcatenate
