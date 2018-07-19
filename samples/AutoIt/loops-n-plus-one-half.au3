#cs ----------------------------------------------------------------------------

 AutoIt Version: 3.3.8.1
 Author:         Alexander Alvonellos

 Script Function:
	Output a comma separated list from 1 to 10, and on the tenth iteration of the
	output loop, only perform half of the loop.

#ce ----------------------------------------------------------------------------

Func doLoopIterative()
		Dim $list = ""
		For $i = 1 To 10 Step 1
			$list = $list & $i
			If($i = 10) Then ExitLoop
			$list = $list & ", "
		Next
		return $list & @CRLF
EndFunc

Func main()
	ConsoleWrite(doLoopIterative())
EndFunc

main()
