	Local $ascarray[4] = [34,38,60,62]
	$String = "Character,Speech" & @CRLF
	$String &= "The multitude,The messiah! Show us the messiah!" & @CRLF
	$String &= "Brians mother,<angry>Now you listen here! He's not the messiah; he's a very naughty boy! Now go away!</angry>" & @CRLF
	$String &= "The multitude,Who are you?" & @CRLF
	$String &= "Brians mother,I'm his mother; that's who!" & @CRLF
	$String &= "The multitude,Behold his mother! Behold his mother!"
	For $i = 0 To UBound($ascarray) -1
		$String = Stringreplace($String, chr($ascarray[$i]), "&#"&$ascarray[$i]&";")
	Next
	$newstring = "<table>" & @CRLF
	$crlfsplit = StringSplit($String, @CRLF, 1)
	For $i = 1 To $crlfsplit[0]
		If $i = 1 Then $newstring &= "<thead>" & @CRLF
		$newstring &= "<tr>" & @CRLF
		$komsplit = StringSplit($crlfsplit[$i], ",")
		For $k = 1 To $komsplit[0]
			If $i = 1 Then
				$newstring &= "<th>" &$komsplit[$k] & "</th>" & @CRLF
			Else
				$newstring &= "<td>" &$komsplit[$k] & "</td>" & @CRLF
			EndIf
		Next
		$newstring &= "</tr>" & @CRLF
		If $i = 1 Then $newstring &= "</thead>" & @CRLF
	Next
	$newstring &= "</table>"
	ConsoleWrite('@@ Debug(' & @ScriptLineNumber & ') : $newstring = ' & $newstring & @crlf & '>Error code: ' & @error & @crlf) ;### Debug Console
