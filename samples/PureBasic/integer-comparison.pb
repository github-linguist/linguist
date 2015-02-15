If OpenConsole()

  Print("Enter an integer: ")
  x.i = Val(Input())
  Print("Enter another integer: ")
  y.i = Val(Input())

  If x < y
    Print( "The first integer is less than the second integer.")
  ElseIf x = y
    Print("The first integer is equal to the second integer.")
  ElseIf x > y
    Print("The first integer is greater than the second integer.")
  EndIf

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit")
  Input()
  CloseConsole()
EndIf
