Dim numbers(20)
For i = 0 To 20
   numbers(i) = Random(1000)
Next

SortArray(numbers(), #PB_Sort_Ascending)
