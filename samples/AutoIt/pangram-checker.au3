Pangram("The quick brown fox jumps over the lazy dog")
Func Pangram($s_String)
For $i = 1 To 26
	IF Not StringInStr($s_String, Chr(64 + $i)) Then
		Return MsgBox(0,"No Pangram", "Character " & Chr(64 + $i) &" is missing")
	EndIf
Next
Return MsgBox(0,"Pangram", "Sentence is a Pangram")
EndFunc
