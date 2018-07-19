DataSection
  ;the list of words that will be added to the list
  words:
  Data.s "One", "Two", "Three", "Four", "Five", "Six", "EndOfData"
EndDataSection


Procedure displayList(List x.s(), title$)
  ;display all elements from list of strings
  Print(title$)
  ForEach x()
    Print(x() + " ")
  Next
  PrintN("")
EndProcedure


OpenConsole()

NewList a.s() ;create a new list of strings

;add words to the head of list
Restore words
Repeat
  Read.s a$
  If a$ <> "EndOfData"
    ResetList(a()) ;Move to head of list
    AddElement(a())
    a() = a$
  EndIf
Until a$ = "EndOfData"
displayList(a(),"Insertion at Head: ")


ClearList(a())
;add words to the tail of list
Restore words
LastElement(a()) ;Move to the tail of the list
Repeat
  Read.s a$
  If a$ <> "EndOfData"
    AddElement(a()) ;after insertion the new position is still at the tail
    a() = a$
  EndIf
Until a$ = "EndOfData"
displayList(a(),"Insertion at Tail: ")


ClearList(a())
;add words to the middle of list
Restore words
ResetList(a()) ;Move to the tail of the list
Repeat
  Read.s a$
  If a$ <> "EndOfData"
    c = CountList(a())
    If c > 1
      SelectElement(a(),Random(c - 2)) ;insert after a random element but before tail
    Else
      FirstElement(a())
    EndIf
    AddElement(a())
    a() = a$
  EndIf
Until a$ = "EndOfData"
displayList(a(),"Insertion in Middle: ")

Repeat: Until Inkey() <> ""
