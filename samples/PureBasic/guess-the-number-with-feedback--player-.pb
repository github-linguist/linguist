min=0
max=100

If OpenConsole()
  PrintN("Think of a number between "+Str(min)+" and "+Str(max)+".")
  PrintN("On every guess of mine you should state whether my guess was")
  PrintN("too high, too low, or equal to your number by typing 'h', 'l', Or '='")
  Repeat
    If max<=min
      PrintN("I think somthing is strange here...")
      Break
    EndIf
    Guess=(max-min)/2+min
    Print("My guess is "+Str(Guess)+",is this correct? "): Respons.s=UCase(Input())
    If Respons="H":     max=Guess-1
    ElseIf Respons="L": min=Guess+1
    ElseIf Respons="="
      PrintN("I did it!")
      Break
    Else
      PrintN("I do not understand that...")
    EndIf
  ForEver
EndIf
