If OpenConsole()
  ; Declare a string and a integer to be used
  Define txt.s, num.i

  Print("Enter a string: ")
  txt=Input()

  Repeat
    Print("Enter the number 75000: ")
    num=Val(Input()) ; Converts the Input to a Value with Val()
  Until num=75000
  ; Check that the user really gives us 75000!

  Print("You made it!")
  Delay(3000): CloseConsole()
EndIf
