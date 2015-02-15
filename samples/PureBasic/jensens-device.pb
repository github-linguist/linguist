Prototype.d func()

Global i

Procedure.d Sum(*i.Integer, lo, hi, *term.func)
  Protected Temp.d
  For i=lo To hi
    temp + *term()
  Next
  ProcedureReturn Temp
EndProcedure

Procedure.d term_func()
  ProcedureReturn 1/i
EndProcedure

Answer.d = Sum(@i, 1, 100, @term_func())
