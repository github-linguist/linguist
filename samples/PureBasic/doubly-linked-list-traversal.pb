NewList MyData.i() ; Create a double linked list holding a single value (integer)

;Set up a randomly long linked list in the range 25-125 elements
For i=0 To (Random(100)+25)
  AddElement(MyData())        ; Create a new tailing element
  MyData()=Random(314)        ; Inert a vale into it
Next
;
;Traverse from the beginning of a doubly-linked list to the end.
FirstElement(MyData())
Repeat
  Debug MyData()              ; Present the value in the current cell
Until Not NextElement(MyData())
;
;Traverse from the end to the beginning.
LastElement(MyData())
Repeat
   Debug MyData()             ; Present the value in the current cell
Until Not PreviousElement(MyData())
