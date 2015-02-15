Func move($n, $from, $to, $via)
	If ($n = 1) Then
		ConsoleWrite(StringFormat("Move disk from pole "&$from&" To pole "&$to&"\n"))
	Else
		move($n - 1, $from, $via, $to)
		move(1, $from, $to, $via)
		move($n - 1, $via, $to, $from)
	EndIf
EndFunc

move(4, 1,2,3)
