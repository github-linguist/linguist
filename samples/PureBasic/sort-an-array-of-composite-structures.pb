Structure MyPair ; Define a structured data type
  Name$
  Value.i
EndStructure

Dim People.MyPair(2)             ; Allocate some elements

People(0)\Name$ = "John"         ; Start filling them in
People(0)\Value = 100

People(1)\Name$ = "Emma"
People(1)\Value = 200

People(2)\Name$ = "Johnny"
People(2)\Value = 175

If OpenConsole()
  Define i
  ; Sort ascending by name
  SortStructuredArray(People(), #PB_Sort_Ascending, OffsetOf(MyPair\Name$), #PB_Sort_String)
  PrintN(#CRLF$+"Sorted ascending by name.")
  For i=0 To 2
    PrintN(People(i)\Name$+" - Value: "+Str(People(i)\Value))
  Next
  ; Sort descending by value
  SortStructuredArray(People(), #PB_Sort_Descending, OffsetOf(MyPair\Value), #PB_Sort_Integer)
  PrintN(#CRLF$+"Sorted descending by value.")
  For i=0 To 2
    PrintN(People(i)\Name$+" - Value: "+Str(People(i)\Value))
  Next
  ; Wait for user...
  PrintN(#CRLF$+"Press ENTER to exit"):Input()
EndIf
