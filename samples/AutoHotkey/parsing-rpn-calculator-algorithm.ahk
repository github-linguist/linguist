evalRPN("3 4 2 * 1 5 - 2 3 ^ ^ / +")
evalRPN(s){
	stack := []
	out := "For RPN expression: '" s "'`r`n`r`nTOKEN`t`tACTION`t`t`tSTACK`r`n"
	Loop Parse, s
		If A_LoopField is number
			t .= A_LoopField
		else
		{
			If t
				stack.Insert(t)
				, out .= t "`tPush num onto top of stack`t" stackShow(stack) "`r`n"
				, t := ""
			If InStr("+-/*^", l := A_LoopField)
			{
				a := stack.Remove(), b := stack.Remove()
				stack.Insert(	 l = "+" ? b + a
						:l = "-" ? b - a
						:l = "*" ? b * a
						:l = "/" ? b / a
						:l = "^" ? b **a
						:0	)
				out .= l "`tApply op " l " to top of stack`t" stackShow(stack) "`r`n"
			}
		}
	r := stack.Remove()
	out .= "`r`n The final output value is: '" r "'"
	clipboard := out
	return r
}
StackShow(stack){
	for each, value in stack
		out .= A_Space value
	return subStr(out, 2)
}
