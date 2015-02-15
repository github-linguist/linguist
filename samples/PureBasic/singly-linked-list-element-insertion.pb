Procedure insertAfter(Value, *node.MyData = #Null)
  Protected *newNode.MyData = AllocateMemory(SizeOf(MyData))
  If *newNode
    If *node
      *newNode\next = *node\next
      *node\next = *newNode
    EndIf
    *newNode\Value = Value
  EndIf
  ProcedureReturn *newNode ;return pointer to newnode
EndProcedure


Define *SL_List.MyData, a = 1, b = 2, c = 3

*SL_List = insertAfter(a) ;start the list
insertAfter(b, *SL_List) ;insert after head of list
insertAfter(c, *SL_List) ;insert after head of list and before tail
