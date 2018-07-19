String := "abcdefghijklmnopqrstuvwxyz"
; also: String = abcdefghijklmnopqrstuvwxyz
n := 12
m := 5

; starting from n characters in and of m length;
subString := SubStr(String, n, m)
; alternative:  StringMid, subString, String, n, m
MsgBox % subString

; starting from n characters in, up to the end of the string;
subString := SubStr(String, n)
; alternative:  StringMid, subString, String, n
MsgBox % subString

; whole string minus last character;
StringTrimRight, subString, String, 1
; alternatives: subString := SubStr(String, 1, StrLen(String) - 1)
;               StringMid, subString, String, 1, StrLen(String) - 1
MsgBox % subString

; starting from a known character within the string and of m length;
findChar := "q"
subString := SubStr(String, InStr(String, findChar), m)
; alternatives: RegExMatch(String, findChar . ".{" . m - 1 . "}", subString)
;               StringMid, subString, String, InStr(String, findChar), m
MsgBox % subString

; starting from a known character within the string and of m length;
findString := "pq"
subString := SubStr(String, InStr(String, findString), m)
; alternatives: RegExMatch(String, findString . ".{" . m - StrLen(findString) . "}", subString)
;               StringMid, subString, String, InStr(String, findString), m
MsgBox % subString
