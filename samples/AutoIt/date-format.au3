#include <Date.au3>

$iYear = 2007
$iMonth = 11
$iDay = 10

ConsoleWrite(StringFormat('%4d-%02d-%02d', $iYear, $iMonth, $iDay) & @LF)

$iWeekDay = _DateToDayOfWeekISO($iYear, $iMonth, $iDay)
ConsoleWrite(StringFormat('%s, %s %02d, %4d', _GetLongDayLocale($iWeekDay), _GetLongMonthLocale($iMonth), $iDay, $iYear) & @LF)


Func _GetLongDayLocale($_iWeekDay)  ; 1..7 Monday=1
	Local $aDayName[8] = [0, 0x2A, 0x2B, 0x2C, 0x2D, 0x2E, 0x2F, 0x30]
	Return GetLocaleInfo($aDayName[$_iWeekDay])
EndFunc

Func _GetLongMonthLocale($_iMonth)  ; 1..12 January=1
	Local $aMonthName[13] = [0, 0x38, 0x39, 0x3A, 0x3B, 0x3C, 0x3D, 0x3E, 0x3F, 0x40, 0x41, 0x42, 0x43]
	Return GetLocaleInfo($aMonthName[$_iMonth])
EndFunc

Func GetLocaleInfo($_LCType)
	Local $ret, $LCID, $sBuffer, $iLen
	$ret = DllCall('kernel32', 'long', 'GetSystemDefaultLCID')
	$LCID = $ret[0]
	$ret = DllCall('kernel32', 'long', 'GetLocaleInfo', 'long', $LCID, 'long', $_LCType, 'str', $sBuffer, 'long', 0)
	$iLen = $ret[0]
	$ret = DllCall('kernel32', 'long', 'GetLocaleInfo', 'long', $LCID, 'long', $_LCType, 'str', $sBuffer, 'long', $iLen)
	Return $ret[3]
EndFunc
