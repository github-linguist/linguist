Procedure insertionSort(Array a(1))
  Protected low, high
  Protected firstIndex, lastIndex = ArraySize(a())

  If lastIndex > firstIndex + 1
    low = firstIndex + 1
    While low <= lastIndex
      high = low
      While high > firstIndex
        If a(high) < a(high - 1)
          Swap a(high), a(high - 1)
        Else
          Break
        EndIf
        high - 1
      Wend
      low + 1
    Wend
  EndIf
EndProcedure
