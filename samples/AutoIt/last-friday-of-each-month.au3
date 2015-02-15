#include <Date.au3>

$iYear = InputBox('Last Friday in each month', 'Please input the year:')

_GetLastFridays($iYear)

Func _GetLastFridays($_iYear)
	Local $sResult = 'last fridays in ' & $_iYear & @LF, $iDay
	Local $aDaysInMonth[12] = [31,28,31,30,31,30,31,31,30,31,30,31]
	If _DateIsLeapYear($_iYear) Then $aDaysInMonth[1] = 29
	For $i = 1 To 12
		$iDay = $aDaysInMonth[$i-1]
		While 1
			If _DateToDayOfWeekISO($_iYear, $i, $iDay) = 5 Then
				$sResult &= StringFormat('%4d-%02d-%02d', $_iYear, $i, $iDay) & @LF
				ExitLoop
			EndIf
			$iDay -= 1
		WEnd
	Next
	ConsoleWrite($sResult)
EndFunc  ;==>_GetFridays
