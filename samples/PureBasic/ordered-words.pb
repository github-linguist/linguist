Procedure.s sortLetters(*word.Character, wordLength) ;returns a string with the letters of a word sorted
  Protected Dim letters.c(wordLength)
  Protected *letAdr = @letters()

  CopyMemoryString(*word, @*letAdr)
  SortArray(letters(), #PB_Sort_Ascending, 0, wordLength - 1)
  ProcedureReturn PeekS(@letters(), wordLength)
EndProcedure

Structure orderedWord
  word.s
  length.i
EndStructure

Define filename.s = "unixdict.txt", fileNum = 0,  word.s

If OpenConsole()
  NewList orderedWords.orderedWord()
  If ReadFile(fileNum, filename)
    While Not Eof(fileNum)
      word = ReadString(fileNum)
      If word = sortLetters(@word, Len(word))
        AddElement(orderedWords())
        orderedWords()\word = word
        orderedWords()\length = Len(word)
      EndIf
    Wend
  EndIf

  SortStructuredList(orderedWords(), #PB_Sort_Ascending, OffsetOf(orderedWord\word), #PB_Sort_String)
  SortStructuredList(orderedWords(), #PB_Sort_Descending, OffsetOf(orderedWord\length), #PB_Sort_integer)
  Define maxLength
  FirstElement(orderedWords())
  maxLength = orderedWords()\length
  ForEach orderedWords()
    If orderedWords()\length = maxLength
      Print(orderedWords()\word + "  ")
    EndIf
  Next

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit"): Input()
  CloseConsole()
EndIf
