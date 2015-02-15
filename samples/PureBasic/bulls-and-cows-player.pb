#answerSize = 4
Structure history
  answer.s
  bulls.i
  cows.i
EndStructure

Procedure evaluateGuesses(*answer.history, List remainingGuesses.s())
  Protected i, cows, bulls

  ForEach remainingGuesses()
    bulls = 0: cows = 0
    For i = 1 To #answerSize
      If Mid(remainingGuesses(), i, 1) = Mid(*answer\answer, i, 1)
        bulls + 1
      ElseIf FindString(remainingGuesses(), Mid(*answer\answer, i, 1), 1)
        cows + 1
      EndIf
    Next
    If bulls <> *answer\bulls Or cows <> *answer\cows
      DeleteElement(remainingGuesses())
    EndIf
  Next
EndProcedure

Procedure findPermutations(List permutations.s(), elementChar.s, permSize)
  Protected i, j, stackDepth, elementCount = Len(elementChar) - 1, working.s = Space(permSize), *working = @working
  permSize - 1
  Dim stack(permSize) ;holds index states

  Dim elements(elementCount)
  Dim elementChar.c(elementCount)
  For i = 0 To elementCount
    elementChar(i) = PeekC(@elementChar + i * SizeOf(Character))
  Next

  i = 0
  Repeat
    While i <= elementCount
      If elements(i) = 0
        stack(stackDepth) = i
        If stackDepth = permSize
          For j = 0 To permSize
            PokeC(*working + j * SizeOf(Character), elementChar(stack(j)))
          Next
          AddElement(permutations())
          permutations() = working
        Else
          elements(i) = 1
          stackDepth + 1
          i = 0
          Continue ;skip update
        EndIf
      EndIf
      i + 1
    Wend
    stackDepth - 1
    If stackDepth < 0
      Break
    EndIf
    i = stack(stackDepth) + 1
    elements(i - 1) = 0
  ForEver
EndProcedure


If OpenConsole()
  Define guess.s, guessNum, score.s, delimeter.s
  NewList remainingGuesses.s()
  NewList answer.history()
  findPermutations(remainingGuesses(), "123456789", 4)

  PrintN("Playing Bulls & Cows with " + Str(#answerSize) + " unique digits." + #CRLF$)
  Repeat
    If ListSize(remainingGuesses()) = 0
      If answer()\bulls = #answerSize And answer()\cows = 0
        PrintN(#CRLF$ + "Solved!")
        Break ;exit Repeat/Forever
      EndIf

      PrintN(#CRLF$ + "BadScoring!  Nothing fits the scores you gave.")
      ForEach answer()
        PrintN(answer()\answer + " -> [" + Str(answer()\bulls) + ", " + Str(answer()\cows) + "]")
      Next
      Break ;exit Repeat/Forever
    Else
      guessNum + 1
      SelectElement(remainingGuesses(), Random(ListSize(remainingGuesses()) - 1))
      guess = remainingGuesses()
      DeleteElement(remainingGuesses())

      Print("Guess #" + Str(guessNum) + " is " + guess + ".  What does it score (bulls, cows)?")
      score = Input()
      If CountString(score, ",") > 0: delimeter = ",": Else: delimeter = " ": EndIf

      AddElement(answer())
      answer()\answer = guess
      answer()\bulls = Val(StringField(score, 1, delimeter))
      answer()\cows = Val(StringField(score, 2, delimeter))
      evaluateGuesses(@answer(), remainingGuesses())
    EndIf
  ForEver

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit"): Input()
  CloseConsole()
EndIf
