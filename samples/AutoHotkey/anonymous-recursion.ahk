Fib(n) {
	nold1 := 1
	nold2 := 0
	If n < 0
	{
		MsgBox, Positive argument required!
		Return
	}
	Else If n = 0
		Return nold2
	Else If n = 1
		Return nold1
	Fib_Label:
	t := nold2+nold1
	If n > 2
	{
		n--
		nold2:=nold1
		nold1:=t
		GoSub Fib_Label
	}
	Return t
}
