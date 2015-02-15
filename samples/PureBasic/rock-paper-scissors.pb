Enumeration
  ;choices are in listed according to their cycle, weaker followed by stronger
  #rock
  #paper
  #scissors
  #numChoices ;this comes after all possible choices
EndEnumeration

;give names to each of the choices
Dim choices.s(#numChoices - 1)
choices(#rock) = "rock"
choices(#paper) = "paper"
choices(#scissors) = "scissors"

Define gameCount
Dim playerChoiceHistory(#numChoices - 1)

Procedure weightedRandomChoice()
  Shared gameCount, playerChoiceHistory()
  Protected x = Random(gameCount - 1), t, c

  For i = 0 To #numChoices - 1
    t + playerChoiceHistory(i)
    If t >= x
      c = i
      Break
    EndIf
  Next

  ProcedureReturn (c + 1) % #numChoices
EndProcedure

If OpenConsole()
  PrintN("Welcome to the game of rock-paper-scissors")
  PrintN("Each player guesses one of these three, and reveals it at the same time.")
  PrintN("Rock blunts scissors, which cut paper, which wraps stone.")
  PrintN("If both players choose the same, it is a draw!")
  PrintN("When you've had enough, choose Q.")

  Define computerChoice, playerChoice, response.s
  Define playerWins, computerWins, draw, quit

  computerChoice = Random(#numChoices - 1)
  Repeat
    Print(#CRLF$ + "What is your move (press R, P, or S)?")
    Repeat
      response = LCase(Input())
    Until FindString("rpsq", response) > 0

    If response = "q":
      quit = 1
    Else
      gameCount + 1
      playerChoice = FindString("rps", response) - 1

      result = (playerChoice - computerChoice + #numChoices) % #numChoices
      Print("You chose " + choices(playerChoice) + " and I chose " + choices(computerChoice))
      Select result
        Case 0
          PrintN(". It's a draw.")
          draw + 1
        Case 1
          PrintN(". You win!")
          playerWins + 1
        Case 2
          PrintN(". I win!")
          computerWins + 1
      EndSelect
      playerChoiceHistory(playerChoice) + 1
      computerChoice = weightedRandomChoice()
    EndIf
  Until quit

  Print(#CRLF$ + "You chose: ")
  For i = 0 To #numChoices - 1
    Print(choices(i) + " " + StrF(playerChoiceHistory(i) * 100 / gameCount, 1) + "%; ")
  Next
  PrintN("")
  PrintN("You won " + Str(playerWins) + ", and I won " + Str(computerWins) + ". There were " + Str(draw) + " draws.")
  PrintN("Thanks for playing!")

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit"): Input()
  CloseConsole()
EndIf
