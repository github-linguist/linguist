Procedure Counting_sort(Array data_array(1), min, max)
  Define i, j
  Dim c(max - min)

  For i = 0 To ArraySize(data_array())
    c(data_array(i) - min) + 1
  Next

  For i = 0 To ArraySize(c())
    While c(i)
      data_array(j) = i + min
      j + 1
      c(i) - 1
    Wend
  Next
EndProcedure
