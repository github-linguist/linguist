#cs ----------------------------------------------------------------------------

 AutoIt Version: 3.3.8.1
 Author:         Alexander Alvonellos


 Script Function:
	Perform primality test on a given integer $number.
	RETURNS: TRUE/FALSE

#ce ----------------------------------------------------------------------------
Func main()
	ConsoleWrite("The primes up to 100 are: " & @LF)
	For $i = 1 To 100 Step 1
		If(isPrime($i)) Then
			If($i <> 97) Then
				ConsoleWrite($i & ", ")
			Else
				ConsoleWrite($i)
			EndIf
		EndIf
	Next
EndFunc

Func isPrime($n)
	If($n < 2) Then Return False
	If($n = 2) Then Return True
	If(BitAnd($n, 1) = 0) Then Return False
	For $i = 3 To Sqrt($n) Step 2
		If(Mod($n, $i) = 0) Then Return False
	Next
	Return True
EndFunc
main()
