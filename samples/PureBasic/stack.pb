Global NewList MyStack()

Procedure Push_LIFO(n)
  FirstElement(MyStack())
  InsertElement(MyStack())
  MyStack() = n
EndProcedure

Procedure Pop_LIFO()
  If FirstElement(MyStack())
    Topmost = MyStack()
    DeleteElement(MyStack())
  EndIf
  ProcedureReturn Topmost
EndProcedure

Procedure Empty_LIFO()
  Protected Result
  If ListSize(MyStack())=0
    Result = #True
  EndIf
  ProcedureReturn Result
EndProcedure

Procedure Peek_LIFO()
  If FirstElement(MyStack())
    Topmost = MyStack()
  EndIf
  ProcedureReturn Topmost
EndProcedure

;----   Example of implementation ----
Push_LIFO(3)
Push_LIFO(1)
Push_LIFO(4)
While Not Empty_LIFO()
  Debug Pop_LIFO()
Wend
