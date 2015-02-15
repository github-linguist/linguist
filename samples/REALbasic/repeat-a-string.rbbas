Function Repeat(s As String, count As Integer) As String
  Dim output As String
  For i As Integer = 0 To count
    output = output + s
  Next
  Return output
End Function
