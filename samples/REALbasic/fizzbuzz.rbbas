  For i As Integer = 1 To 100
    If i mod 3 = 0 And i mod 5 = 0 Then
      Print("FizzBuzz")
    ElseIf i mod 3 = 0 Then
      Print("Fizz")
    ElseIf i mod 5 = 0 Then
      Print("Buzz")
    Else
      Print(Str(i))
    End If
  Next
