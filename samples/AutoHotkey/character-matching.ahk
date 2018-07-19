String1 = abcd
String2 = abab

If (SubStr(String1,1,StrLen(String2)) = String2)
 MsgBox, "%String1%" starts with "%String2%".
IfInString, String1, %String2%
{
 Position := InStr(String1,String2)
 StringReplace, String1, String1, %String2%, %String2%, UseErrorLevel
 MsgBox, "%String1%" contains "%String2%" at position %Position%`, and appears %ErrorLevel% times.
}
StringRight, TempVar, String1, StrLen(String2)
If TempVar = %String2%
 MsgBox, "%String1%" ends with "%String2%".
