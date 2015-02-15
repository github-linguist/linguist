;AutoIt Script Example
;by Daniel Barnes
;spam me at djbarnes at orcon dot net dot en zed
;13/08/2012

;Choose four random digits (1-9) with repetitions allowed:
global $digits
FOR $i = 1 TO 4
	$digits &= Random(1,9,1)
NEXT

While 1
	main()
WEnd

Func main()
	$text  = "Enter an equation (using all of, and only, the single digits "&$digits &")"&@CRLF
	$text &= "which evaluates to exactly 24. Only multiplication (*) division (/)"&@CRLF
	$text &= "addition (+) and subtraction (-) operations and parentheses are allowed:"
	$input = InputBox ("24 Game",$text,"","",400,150)
	If @error Then exit

	;remove any spaces in input
	$input = StringReplace($input," ","")

	;check correct characters were used
	For $i = 1 To StringLen($input)
		$chr = StringMid($input,$i,1)
		If Not StringInStr("123456789*/+-()",$chr) Then
			MsgBox (0, "ERROR","Invalid character used: '"&$chr&"'")
			return
		endif
	Next

	;validate the equation uses all of the 4 characters, and nothing else
	$test = $input
	$test = StringReplace($test,"(","")
	$test = StringReplace($test,")","")

	;validate the length of the input - if its not 7 characters long then the user has done something wrong
	If StringLen ($test) <> 7 Then
		MsgBox (0,"ERROR","The equation "&$test&" is invalid")
		return
	endif

	$test = StringReplace($test,"/","")
	$test = StringReplace($test,"*","")
	$test = StringReplace($test,"-","")
	$test = StringReplace($test,"+","")

	For $i = 1 To StringLen($digits)
		$digit = StringMid($digits,$i,1)
		For $ii = 1 To StringLen($test)
			If  StringMid($test,$ii,1) = $digit Then
				$test = StringLeft($test,$ii-1) & StringRight($test,StringLen($test)-$ii)
				ExitLoop
			endif
		Next
	Next
	If $test <> "" Then
		MsgBox (0, "ERROR", "The equation didn't use all 4 characters, and nothing else!")
		return
	endif

	$try = Execute($input)

	If $try = 24 Then
		MsgBox (0, "24 Game","Well done. Your equation ("&$input&") = 24!")
		Exit
	Else
		MsgBox (0, "24 Game","Fail. Your equation ("&$input&") = "&$try&"!")
		return
	endif
EndFunc
