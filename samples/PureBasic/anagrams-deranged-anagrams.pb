Structure anagram
  word.s
  letters.s
EndStructure

Structure  wordList
  List words.anagram()
EndStructure

#True = 1
#False = 0

Procedure.s sortLetters(*word.Character, wordLength)
  ;returns a string with the letters of a word sorted
  Protected Dim letters.c(wordLength)
  Protected *letAdr = @letters()
  CopyMemoryString(*word, @*letAdr)
  SortArray(letters(), #PB_Sort_Ascending, 0, wordLength - 1)
  ProcedureReturn PeekS(@letters(), wordLength)
EndProcedure

;Compare a list of anagrams for derangement.
Procedure isDeranged(List anagram.s())
  ;If a pair of deranged anagrams is found return #True
  ;and and modify the list to include the pair of deranged anagrams.
  Protected i, length, word.s, *ptrAnagram, isDeranged
  Protected NewList deranged.s()
  FirstElement(anagram())
  length = Len(anagram())
  Repeat
    word = anagram()
    *ptrAnagram = @anagram()

    While NextElement(anagram())
      isDeranged = #True
      For i = 1 To length
        If Mid(word, i, 1) = Mid(anagram(), i, 1)
          isDeranged = #False
          Break ;exit for/next
        EndIf
      Next

      If isDeranged
        AddElement(deranged())
        deranged() = anagram()
        AddElement(deranged())
        deranged() = word
        CopyList(deranged(), anagram())
        ProcedureReturn #True ;deranged anagram found
      EndIf
    Wend
    ChangeCurrentElement(anagram(), *ptrAnagram)
  Until Not NextElement(anagram())

  ProcedureReturn #False ;deranged anagram not found
EndProcedure

If OpenConsole()
  ;word file is assumed to be in the same directory
  If Not ReadFile(0,"unixdict.txt"): End: EndIf

  Define maxWordSize = 0, word.s, length
  Dim wordlists.wordList(maxWordSize)

  ;Read word file and create separate lists of anagrams and their original
  ;words by length.
  While Not Eof(0)
    word = ReadString(0)
    length = Len(word)
    If length > maxWordSize
      maxWordSize = length
      Redim wordlists.wordList(maxWordSize)
    EndIf
    AddElement(wordlists(length)\words())
    wordlists(length)\words()\word = word
    wordlists(length)\words()\letters = sortLetters(@word, length)
  Wend
  CloseFile(0)

  Define offset = OffsetOf(anagram\letters), option = #PB_Sort_Ascending
  Define sortType = #PB_Sort_String
  Define letters.s, foundDeranged
  NewList anagram.s()
  ;start search from largest to smallest
  For length = maxWordSize To 2 Step -1

    If FirstElement(wordlists(length)\words()) ;only examine lists with words
      ;sort words to place anagrams next to each other
      SortStructuredList(wordlists(length)\words(), option, offset, sortType)

      With wordlists(length)\words()
        letters = \letters
        AddElement(anagram()): anagram() = \word

        ;Compose sets of anagrams and check for derangement with remaining
        ;words in current list.
        While NextElement(wordlists(length)\words())
          ;Check for end of a set of anagrams?
          If letters <> \letters

            ;if more than one word in a set of anagrams check for derangement
            If ListSize(anagram()) > 1
              If isDeranged(anagram())
                foundDeranged = #True ;found deranged anagrams, stop processing
                Break 2 ;exit while/wend and for/next
              EndIf
            EndIf

            letters = \letters ;setup for next set of anagrams
            ClearList(anagram())
          EndIf

          AddElement(anagram()): anagram() = \word
        Wend
      EndWith

    EndIf

    ClearList(anagram())
  Next

  ;report results
  If foundDeranged
    Print("Largest 'Deranged' anagrams found are of length ")
    PrintN(Str(length) + ":" + #CRLF$)
    ForEach anagram()
      PrintN("  " + anagram())
    Next
  Else
    PrintN("No 'Deranged' anagrams were found." + #CRLF$)
  EndIf

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit"): Input()
  CloseConsole()
EndIf
