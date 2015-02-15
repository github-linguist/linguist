Procedure GnomeSort(Array a(1))
  Protected Size = ArraySize(a()) + 1
  Protected i = 1, j = 2

  While i < Size
    If a(i - 1) <= a(i)
      ;for descending SORT, use >= for comparison
      i = j
      j + 1
    Else
      Swap a(i - 1), a(i)
      i - 1
      If i = 0
        i = j
        j + 1
      EndIf
    EndIf
  Wend
EndProcedure
