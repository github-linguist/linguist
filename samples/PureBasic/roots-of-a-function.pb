Procedure.d f(x.d)
  ProcedureReturn x*x*x-3*x*x+2*x
EndProcedure

Procedure main()
  OpenConsole()
  Define.d StepSize= 0.001
  Define.d Start=-1, stop=3
  Define.d value=f(start), x=start
  Define.i oldsign=Sign(value)

  If value=0
    PrintN("Root found at "+StrF(start))
  EndIf

  While x<=stop
    value=f(x)
    If Sign(value) <> oldsign
      PrintN("Root found near "+StrF(x))
    ElseIf value = 0
      PrintN("Root found at "+StrF(x))
    EndIf
    oldsign=Sign(value)
    x+StepSize
  Wend
EndProcedure

main()
