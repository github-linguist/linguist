SetBatchLines -1
#NoEnv

expr := "3 + 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3"

output := "Testing string '" expr "'`r`n`r`nToken`tOutput Queue"
           . Space(StrLen(expr)-StrLen("Output Queue")+2) "OP Stack"

; define a stack with semantic .push() and .pop() funcs
stack := {push: func("ObjInsert"), pop: func("ObjRemove"), peek: func("Peek")}

Loop Parse, expr, %A_Space%
{
       token := A_LoopField
       if token is number
               Q .= token A_Space
       if isOp(token){
               o1 := token
               while   isOp(o2 := stack.peek())
                       and ((isLeft(o1)  and Precedence(o1) <= Precedence(o2))
                       or  (isRight(o1) and Precedence(o1) <  Precedence(o2)))
                   Q .= stack.pop() A_Space
               stack.push(o1)
       }
       If ( token = "(" )
               stack.push(token)
       If ( token = ")" )
       {
               While ((t := stack.pop()) != "(") && (t != "")
                       Q .= t A_Space
               if (t = "")
                       throw Exception("Unmatched parenthesis. "
                          . "Character number " A_Index)
       }
       output .= "`r`n" token Space(7) Q Space(StrLen(expr)+2-StrLen(Q))
               . Disp(stack)
}
output .= "`r`n(empty stack to output)"
While (t := stack.pop()) != ""
       if InStr("()", t)
               throw Exception("Unmatched parenthesis.")
       else    Q .= t A_Space, output .= "`r`n" Space(8) Q
                       . Space(StrLen(expr)+2-StrLen(Q)) Disp(stack)
output .= "`r`n`r`nFinal string: '" Q "'"
clipboard := output

isOp(t){
       return (!!InStr("+-*/^", t) && t)
}
Peek(this){
       r := this.Remove(), this.Insert(r)
       return r
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
               o := val . o
       return  o
}
Space(n){
       return n>0 ? A_Space Space(n-1) : ""
}
