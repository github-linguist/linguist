#include <date.au3>
Const $iSunday = 1
For $iYear = 2008 To 2121 Step 1
   If $iSunday = _DateToDayOfWeek($iYear, 12, 25) Then
     ConsoleWrite(StringFormat($iYear & "\n"))
   EndIf
Next
