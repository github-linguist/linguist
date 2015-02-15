Structure taskList
  List description.s()  ;implements FIFO queue
EndStructure

Structure task
  *tl.tList  ;pointer to a list of task descriptions
  Priority.i ;tasks priority, lower value has more priority
EndStructure

Structure priorityQueue
  maxHeapSize.i ;increases as needed
  heapItemCount.i  ;number of elements currently in heap
  Array heap.task(0) ;elements hold FIFO queues ordered by priorities, lowest first
  map heapMap.taskList() ;holds lists of tasks with the same priority that are FIFO queues
EndStructure

Procedure insertPQ(*PQ.priorityQueue, description.s, p)
  If FindMapElement(*PQ\heapMap(), Str(p))
    LastElement(*PQ\heapMap()\description())
    AddElement(*PQ\heapMap()\description())
    *PQ\heapMap()\description() = description
  Else
    Protected *tl.taskList = AddMapElement(*PQ\heapMap(), Str(p))
    AddElement(*tl\description())
    *tl\description() = description

    Protected pos = *PQ\heapItemCount

    *PQ\heapItemCount + 1
    If *PQ\heapItemCount > *PQ\maxHeapSize
      Select *PQ\maxHeapSize
        Case 0
          *PQ\maxHeapSize = 128
        Default
          *PQ\maxHeapSize * 2
      EndSelect
      Redim *PQ\heap.task(*PQ\maxHeapSize)
    EndIf

    While pos > 0 And p < *PQ\heap((pos - 1) / 2)\Priority
      *PQ\heap(pos) = *PQ\heap((pos - 1) / 2)
      pos = (pos - 1) / 2
    Wend

    *PQ\heap(pos)\tl = *tl
    *PQ\heap(pos)\Priority = p
  EndIf
EndProcedure

Procedure.s removePQ(*PQ.priorityQueue)
  Protected *tl.taskList = *PQ\heap(0)\tl, description.s
  FirstElement(*tl\description())
  description = *tl\description()
  If ListSize(*tl\description()) > 1
    DeleteElement(*tl\description())
  Else
    DeleteMapElement(*PQ\heapMap(), Str(*PQ\heap(0)\Priority))

    *PQ\heapItemCount - 1
    *PQ\heap(0) = *PQ\heap(*PQ\heapItemCount)

    Protected pos
    Repeat
      Protected child1 = 2 * pos + 1
      Protected child2 = 2 * pos + 2
      If child1 >= *PQ\heapItemCount
        Break
      EndIf

      Protected smallestChild
      If child2 >= *PQ\heapItemCount
        smallestChild = child1
      ElseIf *PQ\heap(child1)\Priority <= *PQ\heap(child2)\Priority
        smallestChild = child1
      Else
        smallestChild = child2
      EndIf

      If (*PQ\heap(smallestChild)\Priority >= *PQ\heap(pos)\Priority)
        Break
      EndIf
      Swap *PQ\heap(pos)\tl, *PQ\heap(smallestChild)\tl
      Swap *PQ\heap(pos)\Priority, *PQ\heap(smallestChild)\Priority
      pos = smallestChild
    ForEver
  EndIf

  ProcedureReturn description
EndProcedure

Procedure isEmptyPQ(*PQ.priorityQueue) ;returns 1 if empty, otherwise returns 0
  If *PQ\heapItemCount
    ProcedureReturn 0
  EndIf
  ProcedureReturn 1
EndProcedure

If OpenConsole()
  Define PQ.priorityQueue
  insertPQ(PQ, "Clear drains", 3)
  insertPQ(PQ, "Answer Phone 1", 8)
  insertPQ(PQ, "Feed cat", 4)
  insertPQ(PQ, "Answer Phone 2", 8)
  insertPQ(PQ, "Make tea", 5)
  insertPQ(PQ, "Sleep", 9)
  insertPQ(PQ, "Check email", 3)
  insertPQ(PQ, "Solve RC tasks", 1)
  insertPQ(PQ, "Answer Phone 3", 8)
  insertPQ(PQ, "Exercise", 9)
  insertPQ(PQ, "Answer Phone 4", 8)
  insertPQ(PQ, "Tax return", 2)

  While Not isEmptyPQ(PQ)
    PrintN(removePQ(PQ))
  Wend

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit"): Input()
  CloseConsole()
EndIf
