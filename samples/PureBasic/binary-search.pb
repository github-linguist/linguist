#Recursive = 0 ;recursive binary search method
#Iterative = 1 ;iterative binary search method
#NotFound = -1 ;search result if item not found

;Recursive
Procedure  R_BinarySearch(Array a(1), value, low, high)
  Protected mid
  If high < low
    ProcedureReturn #NotFound
  EndIf

  mid = (low + high) / 2
  If a(mid) > value
    ProcedureReturn R_BinarySearch(a(), value, low, mid - 1)
  ElseIf a(mid) < value
    ProcedureReturn R_BinarySearch(a(), value, mid + 1, high)
  Else
    ProcedureReturn mid
  EndIf
EndProcedure

;Iterative
Procedure I_BinarySearch(Array a(1), value, low, high)
  Protected mid
  While low <= high
    mid = (low + high) / 2
    If a(mid) > value
      high = mid - 1
    ElseIf a(mid) < value
      low = mid + 1
    Else
      ProcedureReturn mid
    EndIf
  Wend

  ProcedureReturn #NotFound
EndProcedure

Procedure search (Array a(1), value, method)
  Protected idx

  Select method
    Case #Iterative
      idx = I_BinarySearch(a(), value, 0, ArraySize(a()))
    Default
      idx = R_BinarySearch(a(), value, 0, ArraySize(a()))
  EndSelect

  Print("  Value " + Str(Value))
  If idx < 0
    PrintN(" not found")
  Else
    PrintN(" found at index " + Str(idx))
  EndIf
EndProcedure


#NumElements = 9 ;zero based count
Dim test(#NumElements)

DataSection
  Data.i 2, 3, 5, 6, 8, 10, 11, 15, 19, 20
EndDataSection

;fill the test array
For i = 0 To #NumElements		
  Read test(i)
Next


If OpenConsole()

  PrintN("Recursive search:")
  search(test(), 4, #Recursive)
  search(test(), 8, #Recursive)
  search(test(), 20, #Recursive)

  PrintN("")
  PrintN("Iterative search:")
  search(test(), 4, #Iterative)
  search(test(), 8, #Iterative)
  search(test(), 20, #Iterative)

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit")
  Input()
  CloseConsole()
EndIf
