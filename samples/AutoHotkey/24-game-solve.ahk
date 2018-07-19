#NoEnv
InputBox, NNNN       ; user input 4 digits
NNNN := RegExReplace(NNNN, "(\d)(?=\d)", "$1,") ; separate with commas for the sort command
sort NNNN, d`, ; sort in ascending order for the permutations to work
StringReplace NNNN, NNNN, `,, , All ; remove comma separators after sorting

ops := "+-*/"
patterns := [	 "x x.x.x."
		,"x x.x x.."
		,"x x x..x."
		,"x x x.x.."
		,"x x x x..."	]

; build bruteforce operator list ("+++, ++-, ++* ... ///")
a := b := c := 0
While (++a<5){
 While (++b<5){
  While (++c<5){
   l := SubStr(ops, a, 1) . SubStr(ops, b, 1) . SubStr(ops, c, 1)

   ; build bruteforce template ("x x+x+x+, x x+x x++ ... x x x x///")
   For each, pattern in patterns
   {
      Loop 3
         StringReplace, pattern, pattern, ., % SubStr(l, A_Index, 1)
      pat .= pattern "`n"
   }
  }c := 0
 }b := 0
}
StringTrimRight, pat, pat, 1 ; remove trailing newline


; permutate input. As the lexicographic algorithm is used, each permutation generated is unique
While NNNN
{
	StringSplit, N, NNNN
	; substitute numbers in for x's and evaluate
	Loop Parse, pat, `n
	{
		eval := A_LoopField ; current line
		Loop 4
			StringReplace, eval, eval, x, % N%A_Index% ; substitute number for "x"
		If Round(evalRPN(eval), 4) = 24
			final .= eval "`n"
	}
	NNNN := perm_next(NNNN) ; next lexicographic permutation of user's digits
}
MsgBox % final ? clipboard := final : "No solution"

; simple stack-based evaluation. Integers only. Whitespace is used to push a value.
evalRPN(s){
	stack := []
	Loop Parse, s
		If A_LoopField is number
			t .= A_LoopField
		else
		{
			If t
				stack.Insert(t), t := ""
			If InStr("+-/*", l := A_LoopField)
			{
				a := stack.Remove(), b := stack.Remove()
				stack.Insert(	 l = "+" ? b + a
						:l = "-" ? b - a
						:l = "*" ? b * a
						:l = "/" ? b / a
						:0	)
			}
		}
	return stack.Remove()
}



perm_Next(str){
	p := 0, sLen := StrLen(str)
	Loop % sLen
	{
		If A_Index=1
			continue
		t := SubStr(str, sLen+1-A_Index, 1)
		n := SubStr(str, sLen+2-A_Index, 1)
		If ( t < n )
		{
			p := sLen+1-A_Index, pC := SubStr(str, p, 1)
			break
		}
	}
	If !p
		return false
	Loop
	{
		t := SubStr(str, sLen+1-A_Index, 1)
		If ( t > pC )
		{
			n := sLen+1-A_Index, nC := SubStr(str, n, 1)
			break
		}
	}
	return SubStr(str, 1, p-1) . nC . Reverse(SubStr(str, p+1, n-p-1) . pC .  SubStr(str, n+1))
}

Reverse(s){
	Loop Parse, s
		o := A_LoopField o
	return o
}
