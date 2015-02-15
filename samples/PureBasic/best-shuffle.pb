Structure charInfo
  Char.s
  List Position.i()
  count.i          ;number of occurrences of Char
EndStructure

Structure cycleInfo
  Char.s
  Position.i
EndStructure

Structure cycle
  List cycle.cycleInfo()
EndStructure

Procedure.s shuffleWordLetters(word.s)
  Protected i
  Dim originalLetters.s(len(word) - 1)
  For i = 1 To Len(word)
    originalLetters(i - 1) = Mid(word, i, 1)
  Next

  Dim shuffledLetters.s(0)
  CopyArray(originalLetters(), shuffledLetters())

  ;record original letters and their positions
  Protected curChar.s
  NewList letters.charInfo()
  NewMap *wordInfo.charInfo()
  For i = 0 To ArraySize(originalLetters())
    curChar = originalLetters(i)
    If FindMapElement(*wordInfo(), curChar)
      AddElement(*wordInfo()\position())
      *wordInfo()\position() = i
    Else
      *wordInfo(curChar) = AddElement(letters())
      If *wordInfo()
        *wordInfo()\Char = curChar
        AddElement(*wordInfo()\position())
        *wordInfo()\position() = i
      EndIf
    EndIf
  Next

  ForEach letters()
    letters()\count = ListSize(letters()\Position())
  Next

  SortStructuredList(letters(), #PB_Sort_Ascending, OffsetOf(charInfo\Char), #PB_Sort_String) ;extra sort step, not strictly necessary
  SortStructuredList(letters(), #PB_Sort_Descending, OffsetOf(charInfo\count), #PB_Sort_integer)

  ;construct letter cycles
  FirstElement(letters())
  Protected maxLetterCount = letters()\count
  Dim letterCycles.cycle(maxLetterCount - 1)

  Protected curCycleIndex
  ForEach letters()
    ForEach letters()\Position()
      With letterCycles(curCycleIndex)
        AddElement(\cycle())
        \cycle()\Char = letters()\Char
        \cycle()\Position = letters()\position()
      EndWith
      curCycleIndex = (curCycleIndex + 1) % maxLetterCount
    Next
  Next

  ;rotate letters in each cycle
  Protected isFirst, prevChar.s, pos_1
  For i = 0 To maxLetterCount - 1
    With letterCycles(i)
      isFirst = #True
      ForEach \cycle()
        If Not isFirst
          shuffledLetters(\cycle()\Position) = prevChar
        Else
          pos_1 = \cycle()\Position
          isFirst = #False
        EndIf
        prevChar = \cycle()\Char
      Next
      shuffledLetters(pos_1) = prevChar
    EndWith
  Next

  ;score and display shuffle
  Protected shuffledWord.s, ignored
  For i = 0 To ArraySize(shuffledLetters())
    shuffledWord + shuffledLetters(i)
    If shuffledLetters(i) = originalLetters(i)
      ignored + 1
    EndIf
  Next

  PrintN(word + ", " + shuffledWord + ", (" + Str(ignored) + ")")
  ProcedureReturn shuffledWord
EndProcedure

If OpenConsole()
  shuffleWordLetters("abracadabra")
  shuffleWordLetters("seesaw")
  shuffleWordLetters("elk")
  shuffleWordLetters("grrrrrr")
  shuffleWordLetters("up")
  shuffleWordLetters("a")

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit"): Input()
  CloseConsole()
EndIf
