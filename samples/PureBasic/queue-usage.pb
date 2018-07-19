NewList MyStack()

Procedure Push(n)
  Shared MyStack()
  LastElement(MyStack())
  AddElement(MyStack())
  MyStack()=n
EndProcedure

Procedure Pop()
  Shared MyStack()
  Protected n
  If FirstElement(MyStack())  ; e.g. Stack not empty
    n=MyStack()
    DeleteElement(MyStack(),1)
  EndIf
  ProcedureReturn n
EndProcedure

Procedure Empty()
  Shared MyStack()
  If  ListSize(MyStack())=0
    ProcedureReturn #True
  EndIf
  ProcedureReturn #False
EndProcedure

;----   Example of implementation ----
Push(3)
Push(1)
Push(4)
Push(1)
Push(5)
While Not Empty()
  Debug Pop()
Wend
