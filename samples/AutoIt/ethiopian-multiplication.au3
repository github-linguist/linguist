Func Halve($x)
	Return Int($x/2)
EndFunc

Func Double($x)
	Return ($x*2)
EndFunc

Func IsEven($x)
	Return (Mod($x,2) == 0)
EndFunc

; this version also supports negative parameters
Func Ethiopian($nPlier, $nPlicand, $bTutor = True)
	Local $nResult = 0
	If ($nPlier < 0) Then
		$nPlier =- $nPlier
		$nPlicand =- $nPlicand
	ElseIf ($nPlicand > 0) And ($nPlier > $nPlicand) Then
		$nPlier = $nPlicand
		$nPlicand = $nPlier
	EndIf
	If $bTutor Then _
    ConsoleWrite(StringFormat("Ethiopian multiplication of %d by %d...\n", $nPlier, $nPlicand))
	While ($nPlier >= 1)
		If Not IsEven($nPlier) Then
			$nResult += $nPlicand
			If $bTutor Then ConsoleWrite(StringFormat("%d\t%d\tKeep\n", $nPlier, $nPlicand))
		Else
			If $bTutor Then ConsoleWrite(StringFormat("%d\t%d\tStrike\n", $nPlier, $nPlicand))
		EndIf
		$nPlier = Halve($nPlier)
		$nPlicand = Double($nPlicand)
	WEnd
	If $bTutor Then ConsoleWrite(StringFormat("Answer = %d\n", $nResult))
	Return $nResult
EndFunc

MsgBox(0, "Ethiopian multiplication of 17 by 34", Ethiopian(17, 34) )
