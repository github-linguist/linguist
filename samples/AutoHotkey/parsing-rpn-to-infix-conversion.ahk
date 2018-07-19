expr := "3 4 2 * 1 5 - 2 3 ^ ^ / +"

stack := {push: func("ObjInsert"), pop: func("ObjRemove")}
out := "TOKEN`tACTION                  STACK (comma separated)`r`n"
Loop Parse, expr, %A_Space%
{
	token := A_LoopField
	if token is number
		stack.push([0, token])
	if isOp(token)
	{
		b := stack.pop(), a := stack.pop(), p := b.1 > a.1 ? b.1 : a.1
		p := Precedence(token) > p ? precedence(token) : p
		if (a.1 < b.1) and isRight(token)
			stack.push([p, "( " . a.2 " ) " token " " b.2])
		else if (a.1 > b.1) and isLeft(token)
			stack.push([p, a.2 token " ( " b.2 " ) "])
		else
			stack.push([p, a.2 . " " . token . " " . b.2])
	}
	out .= token "`t" (isOp(token) ? "Push Partial expression "
			   : "Push num" space(16)) disp(stack) "`r`n"
}
out .= "`r`n The final output infix expression is: '" disp(stack) "'"
clipboard := out
isOp(t){
       return (!!InStr("+-*/^", t) && t)
}
IsLeft(o){
       return !!InStr("*/+-", o)
}
IsRight(o){
       return o = "^"
}
Precedence(o){
       return (InStr("+-/*^", o)+3)//2
}
Disp(obj){
       for each, val in obj
		if val[2]
			o .= ", " val[2]
       return  SubStr(o,3)
}
Space(n){
       return n>0 ? A_Space Space(n-1) : ""
}
