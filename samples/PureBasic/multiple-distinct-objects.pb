n=Random(50)+25
Dim A.i(n)
; Creates a Array of n [25-75] elements depending on the outcome of Random().
; Each element will be initiated to zero.

For i=0 To ArraySize(A())
  A(i)=2*i
Next i
; Set each individual element at a wanted (here 2*i) value and
; automatically adjust accordingly to the unknown length of the Array.

NewList *PointersToA()
For i=0 To ArraySize(A())
  AddElement(*PointersToA())
  *PointersToA()=@A(i)
Next
; Create a linked list of the same length as A() above.
; Each element is then set to point to the Array element
; of the same order.

ForEach *PointersToA()
  Debug PeekI(*PointersToA())
Next
; Verify by sending each value of A() via *PointersToA()
; to the debugger's output.
