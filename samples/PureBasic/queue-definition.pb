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
  Else
    Debug "Pop(), out of range. Error at line "+str(#PB_Compiler_Line)
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
While Not Empty()
  Debug Pop()
Wend
;----   Now an extra Pop(), e.g. one to many ----
Debug Pop()
