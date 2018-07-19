Define.d
Prototype.d Func(Time, t)

Procedure.d Euler(*F.Func, y0, a, b, h)
  Protected y=y0, t=a
  While t<=b
    PrintN(RSet(StrF(t,3),7)+" "+RSet(StrF(y,3),7))
    y + h * *F(t,y)
    t + h
  Wend
EndProcedure

Procedure.d newtonCoolingLaw(Time, t)
  ProcedureReturn -0.07*(t-20)
EndProcedure


If OpenConsole()
  Euler(@newtonCoolingLaw(), 100, 0, 100, 2)
  Euler(@newtonCoolingLaw(), 100, 0, 100, 5)
  Euler(@newtonCoolingLaw(), 100, 0, 100,10)

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit"): Input()
  CloseConsole()
EndIf
