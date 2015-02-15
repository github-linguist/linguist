'True=Open; False=Closed
  Dim doors(100) As Boolean   'Booleans default to false
  For j As Integer = 1 To 100
    For i As Integer = 1 to 100
      If i Mod j = 0 Then doors(i) = Not doors(i)
    Next
  Next
