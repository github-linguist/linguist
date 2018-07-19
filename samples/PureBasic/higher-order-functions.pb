Prototype.d func(*text$)

Procedure NumberTwo(arg$)
  Debug arg$
EndProcedure

Procedure NumberOne(*p, text$)
  Define MyFunc.func=*p
  MyFunc(@text$)
EndProcedure

NumberOne(@NumberTwo(),"Hello Worldy!")
