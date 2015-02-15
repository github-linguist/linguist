Define.s secret, guess, c
Define.i bulls, cows, guesses, i

If OpenConsole()

  While Len(secret) < 4
    c = Chr(Random(8) + 49)
    If FindString(secret, c, 1) = 0
      secret + c
    EndIf
  Wend

  Repeat
    Print("Guess a 4-digit number with no duplicate digits: ")
    guess = Input()
    If Len(guess) = 0
      Break   ;break from loop
    EndIf

    isMalformedGuess = #False
    If Len(guess) <> 4
      ;guess is too short
      isMalformedGuess = #True
    Else
      For i = 1 To 4
        c = Mid(guess, i, 1)
        If Not FindString("123456789", c, 1) Or CountString(guess, c) <> 1
          ;guess contains either non-digits or duplicate digits
          isMalformedGuess = #True
          Break ;break from For/Next loop
        EndIf
      Next
    EndIf

    If isMalformedGuess
      PrintN("** You should enter 4 different numeric digits that are each from 1 to 9!")
      Continue ;continue loop
    EndIf

    bulls = 0: cows = 0: guesses = guesses + 1
    For i = 1 To 4
      c = Mid(secret, i, 1)
      If Mid(guess, i, 1) = c
        bulls + 1
      ElseIf FindString(guess, c, 1)
        cows + 1
      EndIf
    Next

    Print( Str(bulls) + " bull")
    If bulls <> 1
      Print( "s")
    EndIf
    Print( ", " + Str(cows) + " cow")
    If cows <> 1
      PrintN( "s")
    Else
      PrintN("")
    EndIf

    If guess = secret
      PrintN("You won after " + Str(guesses) + " guesses!")
      Break    ;break from loop
    EndIf
  ForEver

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit")
  Input()
  CloseConsole()
EndIf
