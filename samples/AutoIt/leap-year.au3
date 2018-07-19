; AutoIt Version: 3.3.8.1
$Year = 2012
$sNot = " not"

If IsLeapYear($Year) Then $sNot = ""
ConsoleWrite ($Year & " is" & $sNot & " a leap year." & @LF)

Func IsLeapYear($_year)
	Return Not Mod($_year, 4) And (Mod($_year, 100) Or Not Mod($_year, 400))
EndFunc

; == But it exists the standard UDF "Date.au3" with this function: "_IsLeapYear($Year)"
