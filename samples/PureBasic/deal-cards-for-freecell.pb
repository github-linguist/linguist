#MaxCardNum = 51 ;zero-based count of cards in a deck
Global deckSize
Global Dim cards(#MaxCardNum) ;card with highest index is at the top of deck

Procedure RNG(seed.q = -1)
  Static state.q
  If seed >= 0
    state = seed
  Else
    state = (state * 214013 + 2531011) % (1 << 31)
    ProcedureReturn state >> 16
  EndIf
EndProcedure

Procedure makeDeck(hand)
  Protected i, c
  For i = 0 To #MaxCardNum: cards(i) = i: Next

  RNG(hand) ;set seed value
  deckSize = #MaxCardNum
  While deckSize
    c = RNG() % (deckSize + 1)
    Swap cards(c), cards(deckSize)
    deckSize - 1
  Wend
  deckSize = #MaxCardNum
EndProcedure

Procedure showDeck(hand)
  Protected i, c
  PrintN("Hand #" + Str(hand))
  makeDeck(hand)
  For i = 0 To #MaxCardNum
    c = cards(#MaxCardNum - i)
    Print(" " + Mid("A23456789TJQK", (c / 4) + 1, 1) + Mid("CDHS",(c % 4) + 1, 1))
    If (i + 1) % 8 = 0 Or i = #MaxCardNum: PrintN(""): EndIf
  Next
EndProcedure

If OpenConsole()
  showDeck(1)
  showDeck(617)
  showDeck(11982)

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit"): Input()
  CloseConsole()
EndIf
