$ww = ""
$ww &= "tH........." & @CR
$ww &= ".   .      " & @CR
$ww &= "   ...     " & @CR
$ww &= ".   .      " & @CR
$ww &= "Ht.. ......"
$rows = StringSplit($ww, @CR)
$cols = StringSplit($rows[1], "")
Global $Wireworldarray[$rows[0]][$cols[0]]
For $I = 1 To $rows[0]
	$cols = StringSplit($rows[$I], "")
	For $k = 1 To $cols[0]
		$Wireworldarray[$I - 1][$k - 1] = $cols[$k]
	Next
Next
Wireworld($Wireworldarray)
Func Wireworld($array)
	Local $labelarray = $array
	Local $Top = 0, $Left = 0
	$hFui = GUICreate("Wireworld", UBound($array, 2) * 25, UBound($array) * 25)
	For $I = 0 To UBound($array) - 1
		For $k = 0 To UBound($array, 2) - 1
			Switch $array[$I][$k]
				Case "t" ; Tail
					$labelarray[$I][$k] = GUICtrlCreateButton("", $Left, $Top, 25, 25)
					GUICtrlSetBkColor($labelarray[$I][$k], 0xFF0000)
				Case "h" ; Head
					$labelarray[$I][$k] = GUICtrlCreateButton("", $Left, $Top, 25, 25)
					GUICtrlSetBkColor($labelarray[$I][$k], 0x0000FF)
				Case "." ; Conductor
					$labelarray[$I][$k] = GUICtrlCreateButton("", $Left, $Top, 25, 25)
					GUICtrlSetBkColor($labelarray[$I][$k], 0xFFFF00)
				Case " " ; Empty
					$labelarray[$I][$k] = GUICtrlCreateButton("", $Left, $Top, 25, 25)
					GUICtrlSetBkColor($labelarray[$I][$k], 0x000000)
			EndSwitch
			$Left += 25
		Next
		$Left = 0
		$Top += 25
	Next
	GUISetState()
	Local $nextsteparray = $array
	While 1
		$msg = GUIGetMsg()
		$array = $nextsteparray
		Sleep(250)
		For $I = 0 To UBound($array) - 1
			For $k = 0 To UBound($array, 2) - 1
				If $array[$I][$k] = " " Then ContinueLoop
				If $array[$I][$k] = "h" Then $nextsteparray[$I][$k] = "t"
				If $array[$I][$k] = "t" Then $nextsteparray[$I][$k] = "."
				If $array[$I][$k] = "." Then
					$counter = 0
					If $I - 1 >= 0 Then ; Top
						If $array[$I - 1][$k] = "h" Then $counter += 1
					EndIf
					If $k - 1 >= 0 Then ; left
						If $array[$I][$k - 1] = "h" Then $counter += 1
					EndIf
					If $I + 1 <= UBound($array) - 1 Then ; Bottom
						If $array[$I + 1][$k] = "h" Then $counter += 1
					EndIf
					If $k + 1 <= UBound($array, 2) - 1 Then ;Right
						If $array[$I][$k + 1] = "h" Then $counter += 1
					EndIf
					If $I - 1 >= 0 And $k - 1 >= 0 Then ; left Top
						If $array[$I - 1][$k - 1] = "h" Then $counter += 1
					EndIf
					If $I + 1 <= UBound($array) - 1 And $k + 1 <= UBound($array, 2) - 1 Then ; Right Bottom
						If $array[$I + 1][$k + 1] = "h" Then $counter += 1
					EndIf
					If $I + 1 <= UBound($array) - 1 And $k - 1 >= 0 Then ;Left Bottom
						If $array[$I + 1][$k - 1] = "h" Then $counter += 1
					EndIf
					If $I - 1 >= 0 And $k + 1 <= UBound($array, 2) - 1 Then ; Top Right
						If $array[$I - 1][$k + 1] = "h" Then $counter += 1
					EndIf
					If $counter = 1 Or $counter = 2 Then $nextsteparray[$I][$k] = "h"
				EndIf
			Next
		Next
		For $I = 0 To UBound($nextsteparray) - 1
			For $k = 0 To UBound($nextsteparray, 2) - 1
				Switch $nextsteparray[$I][$k]
					Case "t" ; Tail
						GUICtrlSetBkColor($labelarray[$I][$k], 0xFF0000)
					Case "h" ; Head
						GUICtrlSetBkColor($labelarray[$I][$k], 0x0000FF)
					Case "." ; Conductor
						GUICtrlSetBkColor($labelarray[$I][$k], 0xFFFF00)
					Case " " ; Empty
						GUICtrlSetBkColor($labelarray[$I][$k], 0x000000)
				EndSwitch
				$Left += 25
			Next
			$Left = 0
			$Top += 25
		Next
		If $msg = -3 Then Exit
	WEnd
EndFunc   ;==>Wireworld
