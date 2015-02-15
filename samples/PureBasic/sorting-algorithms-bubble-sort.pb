Procedure bubbleSort(Array a(1))
  Protected i, itemCount, hasChanged

  itemCount = ArraySize(a())
  Repeat
    hasChanged = #False
    itemCount - 1
    For i = 0 To itemCount
      If a(i) > a(i + 1)
        Swap a(i), a(i + 1)
        hasChanged = #True
      EndIf
    Next
  Until hasChanged = #False
EndProcedure
