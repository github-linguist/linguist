$string1 = "arduinoardblobard"
$string2 = "ard"

; == Determining if the first string starts with second string
If StringLeft($string1, StringLen($string2)) = $string2 Then
	ConsoleWrite("1st string starts with 2nd string." & @CRLF)
Else
	ConsoleWrite("1st string does'nt starts with 2nd string." & @CRLF)
EndIf

; == Determining if the first string contains the second string at any location
; == Print the location of the match for part 2
; == Handle multiple occurrences of a string for part 2
$start = 1
$count = 0
$pos = StringInStr($string1, $string2)
While $pos
	$count += 1
	ConsoleWrite("1st string contains 2nd string at position: " & $pos & @CRLF)
	$pos = StringInStr($string1, $string2, 0, 1, $start + $pos + StringLen($string2))
WEnd
If $count = 0 Then ConsoleWrite("1st string does'nt contain 2nd string." & @CRLF)

; == Determining if the first string ends with the second string
If StringRight($string1, StringLen($string2)) = $string2 Then
	ConsoleWrite("1st string ends with 2nd string." & @CRLF)
Else
	ConsoleWrite("1st string does'nt ends with 2nd string." & @CRLF)
EndIf
