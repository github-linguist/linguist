Structure node
  *prev.node
  *next.node
  value.c ;use character type for elements in this example
EndStructure

Procedure insertAfter(element.c, *c.node)
  ;insert new node *n after node *c and set it's value to element
  Protected *n.node = AllocateMemory(SizeOf(node))
  If *n
    *n\value = element
    *n\prev = *c
    If *c
      *n\next = *c\next
      *c\next = *n
    EndIf
  EndIf
  ProcedureReturn *n
EndProcedure

Procedure displayList(*dl.node)
  Protected *n.node = *dl
  Repeat
    Print(Chr(*n\Value) + " ")
    *n = *n\next
  Until *n = #Null
EndProcedure

If OpenConsole()
  Define dl.node, *n.node

  *n = insertAfter('A',dl)
  insertAfter('B',*n)
  insertAfter('C',*n)

  displayList(dl)

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit")
  Input()
  CloseConsole()
EndIf
