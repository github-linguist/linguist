Structure node
  value.i
  *left.node
  *right.node
EndStructure

Structure queue
  List q.i()
EndStructure

DataSection
  tree:
  Data.s "1(2(4(7),5),3(6(8,9)))"
EndDataSection

;Convenient routine to interpret string data to construct a tree of integers.
Procedure createTree(*n.node, *tPtr.Character)
  Protected num.s, *l.node, *ntPtr.Character

  Repeat
    Select *tPtr\c
      Case '0' To '9'
        num + Chr(*tPtr\c)
      Case '('
        *n\value = Val(num): num = ""
        *ntPtr = *tPtr + 1
        If *ntPtr\c = ','
          ProcedureReturn *tPtr
        Else
          *l = AllocateMemory(SizeOf(node))
          *n\left = *l: *tPtr = createTree(*l, *ntPtr)
        EndIf
      Case ')', ',', #Null
        If num: *n\value = Val(num): EndIf
        ProcedureReturn *tPtr
    EndSelect

    If *tPtr\c = ','
      *l = AllocateMemory(SizeOf(node)):
      *n\right = *l: *tPtr = createTree(*l, *tPtr + 1)
    EndIf
    *tPtr + 1
  ForEver
EndProcedure

Procedure enqueue(List q.i(), element)
  LastElement(q())
  AddElement(q())
  q() = element
EndProcedure

Procedure dequeue(List q.i())
  Protected element
  If FirstElement(q())
    element = q()
    DeleteElement(q())
  EndIf
  ProcedureReturn element
EndProcedure

Procedure onVisit(*n.node)
  Print(Str(*n\value) + " ")
EndProcedure

Procedure preorder(*n.node) ;recursive
  onVisit(*n)
  If *n\left
    preorder(*n\left)
  EndIf
  If *n\right
    preorder(*n\right)
  EndIf
EndProcedure

Procedure inorder(*n.node) ;recursive
  If *n\left
    inorder(*n\left)
  EndIf
  onVisit(*n)
  If *n\right
    inorder(*n\right)
  EndIf
EndProcedure

Procedure postorder(*n.node) ;recursive
  If *n\left
    postorder(*n\left)
  EndIf
  If *n\right
    postorder(*n\right)
  EndIf
  onVisit(*n)
EndProcedure

Procedure levelorder(*n.node)
  Dim q.queue(1)
  Protected readQueue = 1, writeQueue, *currNode.node

  enqueue(q(writeQueue)\q(),*n) ;start queue off with root
  Repeat
    readQueue ! 1: writeQueue ! 1
    While ListSize(q(readQueue)\q())
      *currNode = dequeue(q(readQueue)\q())
      If *currNode\left
        enqueue(q(writeQueue)\q(),*currNode\left)
      EndIf
      If *currNode\right
        enqueue(q(writeQueue)\q(),*currNode\right)
      EndIf
      onVisit(*currNode)
    Wend
  Until ListSize(q(writeQueue)\q()) = 0
EndProcedure

If OpenConsole()
  Define root.node
  createTree(root,?tree)

  Print("preorder: ")
  preorder(root)
  PrintN("")
  Print("inorder: ")
  inorder(root)
  PrintN("")
  Print("postorder: ")
  postorder(root)
  PrintN("")
  Print("levelorder: ")
  levelorder(root)
  PrintN("")

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit")
  Input()
  CloseConsole()
EndIf
