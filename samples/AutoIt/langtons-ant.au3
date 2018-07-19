Global $iCountMax = 100000
Global $aFields[100][100][2]
Global $iDelayStep = 10  ; stop between steps in msec

Global $aDirection[4][4] = [ _ ; [ direction 0-3 ][ left change x, y, right change x, y ]
[-1,  0, +1,  0], _   ; == direction 0
[ 0, -1,  0, +1], _   ; == direction 1
[+1,  0, -1,  0], _   ; == direction 2
[ 0, +1,  0, -1]]     ; == direction 3

Global $hGui = GUICreate("Langton's ant", 100*8, 100*8)
GUISetBkColor(0xFFFFFF)

For $i = 0 To 99
	For $j = 0 To 99
		$aFields[$i][$j][0] = GUICtrlCreateLabel('', $j*8, $i*8)
		GUICtrlSetColor(-1, 0xFF0000)
		$aFields[$i][$j][1] = 0
	Next
Next

GUISetState()

GUICtrlSetData($aFields[49][49][0], '#')

Do
	Sleep($iDelayStep)
Until Not _SetAnt()

Do
Until GUIGetMsg() = -3


Func _SetAnt()
	Local Static $iRowLast = 49, $iColLast = 49, $iCount = 0
	Local Static $aCol[2] = [0xFFFFFF,0x000000], $iDirection = 0
	Local $iRow, $iCol, $fRight = False
	If $iCount = $iCountMax Then Return 0

	; == get current color
	Local $iLastColor = $aFields[$iRowLast][$iColLast][1]

	; == go to left/right
	If $iLastColor = 0 Then $fRight = True

	; == set the ant to the next field
	Local $indexX = 0, $indexY = 1
	If $fRight Then
		$indexX = 2
		$indexY = 3
	EndIf
	$iRow = $iRowLast + ($aDirection[$iDirection][$indexX])
	$iCol = $iColLast + ($aDirection[$iDirection][$indexY])
	If $iRow < 0 Or $iRow > 99 Or $iCol < 0 Or $iCol > 99 Then Return 0
	GUICtrlSetData($aFields[$iRowLast][$iColLast][0], '')
	GUICtrlSetData($aFields[$iRow][$iCol][0], '#')

	; == direction for next step
	If $fRight Then
		$iDirection += 1
		If $iDirection = 4 Then $iDirection = 0
	Else
		$iDirection -= 1
		If $iDirection = -1 Then $iDirection = 3
	EndIf

	; == change the color of the current field
	GUICtrlSetBkColor($aFields[$iRowLast][$iColLast][0], $aCol[(Not $iLastColor)*1])
	$aFields[$iRowLast][$iColLast][1] = (Not $iLastColor)*1

	$iRowLast = $iRow
	$iColLast = $iCol
	$iCount += 1
	WinSetTitle($hGui, '', "Langton's ant      [ step: " & StringFormat('%06d', $iCount) & " ]")
	Return 1
EndFunc  ;==>_SetAnt
