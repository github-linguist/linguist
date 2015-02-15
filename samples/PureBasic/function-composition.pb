;Declare how our function looks like
Prototype.i Func(Arg.i)

; Make a procedure that composes any functions of type "Func"
Procedure Compose(*a.Func,*b.Func, x)
  ProcedureReturn *a(*b(x))
EndProcedure

; Just a procedure fitting "Func"
Procedure f(n)
  ProcedureReturn 2*n
EndProcedure

; Yet another procedure fitting "Func"
Procedure g(n)
  ProcedureReturn n+1
EndProcedure

;- Test it
X=Random(100)
Title$="With x="+Str(x)
Body$="Compose(f(),g(), x) ="+Str(Compose(@f(),@g(),X))
MessageRequester(Title$,Body$)
