#include <Date.au3>
#include <Array.au3>
$array = Five_weekends(1)
_ArrayDisplay($array)
$array = Five_weekends(2)
_ArrayDisplay($array)
$array = Five_weekends(3)
_ArrayDisplay($array)

Func Five_weekends($ret = 1)
	If $ret < 1 Or $ret > 3 Then Return SetError(1, 0, 0)
	Local $avDateArray[1]
	Local $avYearArray[1]
	Local $avMonthArray[1]
	For $iYear = 1900 To 2100
		Local $checkyear = False
		For $iMonth = 1 To 12
			If _DateDaysInMonth($iYear, $iMonth) <> 31 Then ContinueLoop ; Month has less then 31 Days
			If _DateToDayOfWeek($iYear, $iMonth, "01") <> 6 Then ContinueLoop ;First Day is not a Friday
			_ArrayAdd($avMonthArray, $iYear & "-" & _DateToMonth($iMonth))
			$checkyear = True
			For $s = 1 To 31
				Local $Date = _DateToDayOfWeek($iYear, $iMonth, $s)
				If $Date = 6 Or $Date = 7 Or $Date = 1 Then ; if Date is Friday, Saturday or Sunday
					_ArrayAdd($avDateArray, $iYear & "\" & StringFormat("%02d", $iMonth) & "\" & StringFormat("%02d", $s))
				EndIf
			Next
		Next
		If Not $checkyear Then _ArrayAdd($avYearArray, $iYear)
	Next
	$avDateArray[0] = UBound($avDateArray) - 1
	$avYearArray[0] = UBound($avYearArray) - 1
	$avMonthArray[0] = UBound($avMonthArray) - 1
	If $ret = 1 Then
		Return $avDateArray
	ElseIf $ret = 2 Then
		Return $avYearArray
	ElseIf $ret = 3 Then
		Return $avMonthArray
	EndIf
EndFunc   ;==>Five_weekends
