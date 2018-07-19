#include <Date.au3>

; Set the count of characters in each line, minimum is 20 - one month.
Global $iPrintSize = 132

; Set the count of months, you want to print side by side. With "0" it calculates automatically.
; The number will corrected, if it not allowed to print in an rectangle.
; If your print size is to small for given count, it will set back to automatically calculation.
Global $iSideBySide = 3

; Set the count of spaces between months.
Global $iSpace = 4

_CreateCalendar( 1969 )


Func _CreateCalendar($_iYear)
	Local $aMon[12] = ['       January      ', '      February      ', '        March       ', _
	                   '        April       ', '         May        ', '        June        ', _
	                   '        July        ', '       August       ', '      September     ', _
	                   '       October      ', '      November      ', '      December      ']
	Local $sHead = 'Mo Tu We Th Fr Sa Su'
	Local $aDaysInMonth[12] = [31,28,31,30,31,30,31,31,30,31,30,31]
	If _DateIsLeapYear($_iYear) Then $aDaysInMonth[1] = 29

	; == assign date in weekday table for the whole year
	Local $aAllDaysInMonth[6][7][12]   ; [ lines ][ weekdays ][ months ]
	Local $iDay = 1, $iShift
	For $i = 1 To 12
		$iShift = _DateToDayOfWeekISO($_iYear, $i, 1) -1
		For $j = 0 To 5
			For $k = $iShift To 6
				$aAllDaysInMonth[$j][$k][$i-1] = $iDay
				$iDay += 1
				If $iDay > $aDaysInMonth[$i-1] Then ExitLoop(2)
			Next
			$iShift = 0
		Next
		$iDay = 1
	Next

	; == check given side by side count, calculate if needed
	If $iSideBySide > 0 Then
		If $iPrintSize < ($iSideBySide *(20 +$iSpace) -$iSpace) Then $iSideBySide = 0
	EndIf
	Switch $iSideBySide
		Case 0
			$iSideBySide = Int($iPrintSize /(20 +$iSpace))
			If $iPrintSize < 20 Then Return _PrintLine('Escape: Size Error')
			If $iPrintSize < (20 +$iSpace) Then $iSideBySide = 1
		Case 5
			$iSideBySide = 4
		Case 7 To 11
			$iSideBySide = 6
	EndSwitch

	; == create space string
	Local $sSpace = ''
	For $i = 1 To $iSpace
		$sSpace &= ' '
	Next

	; == print header
	_PrintLine(@LF)
	_PrintLine('[ here is Snoopy ]', @LF)
	_PrintLine(StringRegExpReplace($_iYear, '(\d)(\d)(\d)(\d)', '$1 $2 $3 $4'), @LF)

	; == create data for each line, in dependence to count of months in one line
	Local $sLine, $iRight, $sTmp1, $sTmp2
	For $n = 0 To 12 /$iSideBySide -1
		$sTmp1 = ''
		$sTmp2 = ''
		For $z = 0 To $iSideBySide -1
			$sTmp1 &= $aMon[$iSideBySide *$n+$z] & $sSpace
			$sTmp2 &= $sHead & $sSpace
		Next
		_PrintLine(StringTrimRight($sTmp1, $iSpace))
		_PrintLine(StringTrimRight($sTmp2, $iSpace))
		For $j = 0 To 5
			$sLine = ''
			For $i = 1 To $iSideBySide
				For $k = 0 To 6
					$iRight = 3
					If $k = 0 Then $iRight = 2
					$sLine &= StringRight('   ' & $aAllDaysInMonth[$j][$k][$iSideBySide*$n+$i-1], $iRight)
				Next
				If $i < $iSideBySide Then $sLine &= $sSpace
			Next
			_PrintLine($sLine)
		Next
	Next
EndFunc  ;==>_CreateCalendar

Func _PrintLine($_sLine, $_sLF='')
	Local $iLen = StringLen($_sLine)
	Local $sSpace = '', $sLeft = ''
	For $i = 1 To $iPrintSize-1
		$sSpace &= ' '
	Next
	If $iLen < $iPrintSize Then $sLeft = StringLeft($sSpace, Int(($iPrintSize-$iLen)/2))
	ConsoleWrite($sLeft & $_sLine & $_sLF & @LF)
EndFunc  ;==>_PrintLine
