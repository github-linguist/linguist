;sorts an array of integers
Procedure cocktailSort(Array a(1))
  Protected index, hasChanged, low, high

  low = 0
  high = ArraySize(a()) - 1
  Repeat
    hasChanged = #False
    For index = low To high
      If a(index) > a(index + 1)
        Swap a(index), a(index + 1)
        hasChanged = #True
      EndIf
    Next
    high - 1

    If hasChanged = #False
      Break ;we can exit the outer loop here if no changes were made
    EndIf


    hasChanged = #False
    For index = high To low Step -1
      If a(index) > a(index + 1)
        Swap a(index), a(index + 1)
        hasChanged = #True
      EndIf
    Next
    low + 1
  Until hasChanged = #False ;if no elements have been changed, then the array is sorted
EndProcedure
