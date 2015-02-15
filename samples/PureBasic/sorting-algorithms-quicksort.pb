Procedure qSort(Array a(1), firstIndex, lastIndex)
  Protected  low, high, pivotValue

  low = firstIndex
  high = lastIndex
  pivotValue = a((firstIndex + lastIndex) / 2)

  Repeat

    While a(low) < pivotValue
      low + 1
    Wend

    While a(high) > pivotValue
      high - 1
    Wend

    If low <= high
      Swap a(low), a(high)
      low + 1
      high - 1
    EndIf

  Until low > high

  If firstIndex < high
    qSort(a(), firstIndex, high)
  EndIf

  If low < lastIndex
    qSort(a(), low, lastIndex)
  EndIf
EndProcedure

Procedure quickSort(Array a(1))
  qSort(a(),0,ArraySize(a()))
EndProcedure
