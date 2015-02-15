Declare heapify(Array a(1), count)
Declare siftDown(Array a(1), start, ending)

Procedure heapSort(Array a(1), count)
  Protected ending=count-1
  heapify(a(), count)
  While ending>0
    Swap a(ending),a(0)
    siftDown(a(), 0, ending-1)
    ending-1
  Wend
EndProcedure

Procedure heapify(Array a(1), count)
  Protected start=(count-2)/2
  While start>=0
    siftDown(a(),start,count-1)
    start-1
  Wend
EndProcedure

Procedure siftDown(Array a(1), start, ending)
  Protected root=start, child
  While (root*2+1)<=ending
    child=root*2+1
    If child+1<=ending And a(child)<a(child+1)
      child+1
    EndIf
    If a(root)<a(child)
      Swap a(root), a(child)
      root=child
    Else
      Break
    EndIf
  Wend
EndProcedure
