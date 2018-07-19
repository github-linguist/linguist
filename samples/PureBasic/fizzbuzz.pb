OpenConsole()
For x = 1 To 100
  If x%15 = 0
    PrintN("FizzBuzz")
  ElseIf x%3 = 0
    PrintN("Fizz")
  ElseIf x%5 = 0
    PrintN("Buzz")
  Else
    PrintN(Str(x))
  EndIf
Next
Input()
