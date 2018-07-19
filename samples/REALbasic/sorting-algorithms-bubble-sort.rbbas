  Dim sortable() As Integer ' assume the array is populated
  sortable.Shuffle() ' sortable is now randomized
  Do
    Dim swapped As Boolean
    Dim index, bound As Integer
    bound = sortable.Ubound

    While index < bound
      If Sortable(index) > Sortable(index + 1) Then
        Dim s As Integer = Sortable(index)
        Sortable.Remove(index)
        Sortable.Insert(index + 1, s)
        swapped = True
      End If
      index = index + 1
    Wend

    If not swapped Then Exit Do
  Loop
'sortable is now sorted
