Procedure Three()
  a=7
  ShowCallstack()
  CallDebugger
EndProcedure

Procedure Two()
  a=4
  Three()
EndProcedure

Procedure One()
  a=2
  Two()
EndProcedure

One()
