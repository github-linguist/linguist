OpenConsole()

Repeat
  ; Ask for limits, with sanity check
  Print("Enter low limit : "): low  =Val(Input())
  Print("Enter high limit: "): High =Val(Input())
Until High>low

TheNumber=Random(High-low)+low
Debug TheNumber
Repeat
  Print("Guess what number I have: "): Guess=Val(Input())
  If Guess=TheNumber
    PrintN("You got it!"): Break
  ElseIf Guess < TheNumber
    PrintN("Your guess is to low.")
  Else
    PrintN("Your guess is to high.")
  EndIf
ForEver
