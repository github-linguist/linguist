Procedure selectionSort(Array a(1))
  Protected i, j, lastIndex, minIndex

  lastIndex = ArraySize(a())
  For i = 0 To lastIndex - 1
    minIndex = i
    For j = i + 1 To lastIndex
      If a(minIndex) > a(j)
        minIndex = j
      EndIf
    Next
    Swap a(minIndex), a(i)
  Next
EndProcedure
