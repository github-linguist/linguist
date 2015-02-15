CompilerIf #PB_Compiler_Debugger
  CompilerError "Turn off the debugger if you want reasonable speed in this example."
CompilerEndIf

Define.q

Procedure Factor(Number, List Factors())
  Protected I = 3
  While Number % 2 = 0
    AddElement(Factors())
    Factors() = 2
    Number / 2
  Wend
  Protected Max = Number
  While I <= Max And Number > 1
    While Number % I = 0
      AddElement(Factors())
      Factors() = I
      Number/I
    Wend
    I + 2
  Wend
EndProcedure

Number = 9007199254740991
NewList Factors()
time = ElapsedMilliseconds()
Factor(Number, Factors())
time = ElapsedMilliseconds()-time
S.s = "Factored " + Str(Number) + " in " + StrD(time/1000, 2) + " seconds."
ForEach Factors()
  S + #CRLF$ + Str(Factors())
Next
MessageRequester("", S)
