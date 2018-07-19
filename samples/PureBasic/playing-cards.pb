#MaxCards = 52 ;Max Cards in a deck
Structure card
  pip.s
  suit.s
EndStructure

Structure _membersDeckClass
  *vtable.i
  size.i ;zero based count of cards present
  cards.card[#MaxCards] ;deck content
EndStructure

Interface deckObject
  Init()
  shuffle()
  deal.s(isAbbr = #True)
  show(isAbbr = #True)
EndInterface

Procedure.s _formatCardInfo(*card.card, isAbbr = #True)
  ;isAbbr determines if the card information is abbrieviated to 2 characters
  Static pips.s = "2 3 4 5 6 7 8 9 10 Jack Queen King Ace"
  Static suits.s = "Diamonds Clubs Hearts Spades"
  Protected c.s

  If isAbbr
    c = *card\pip + *card\suit
  Else
    c = StringField(pips,FindString("23456789TJQKA", *card\pip, 1), " ") + " of "
    c + StringField(suits,FindString("DCHS", *card\suit, 1)," ")
  EndIf
  ProcedureReturn c
EndProcedure

Procedure setInitialValues(*this._membersDeckClass)
  Protected i, c.s

  Restore cardDat
  For i = 0 To #MaxCards - 1
    Read.s c
    *this\cards[i]\pip = Left(c, 1)
    *this\cards[i]\suit = Right(c, 1)
  Next
EndProcedure

Procedure.s dealCard(*this._membersDeckClass, isAbbr)
  ;isAbbr is #True if the card dealt is abbrieviated to 2 characters
  Protected c.card
  If *this\size < 0
    ;deck is empty
    ProcedureReturn ""
  Else
    c = *this\cards[*this\size]
    *this\size - 1
    ProcedureReturn _formatCardInfo(@c, isAbbr)
  EndIf
EndProcedure

Procedure showDeck(*this._membersDeckClass, isAbbr)
  ;isAbbr determines if cards are shown with 2 character abbrieviations
  Protected i

  For i = 0 To *this\size
    Print(_formatCardInfo(@*this\cards[i], isAbbr))
    If i <> *this\size: Print(", "): EndIf
  Next
  PrintN("")
EndProcedure

Procedure shuffle(*this._membersDeckClass)
  ;works with decks of any size
  Protected w, i
  Dim shuffled.card(*this\size)

  For i = *this\size To 0 Step -1
    w = Random(i)
    shuffled(i) = *this\cards[w]
    If w <> i
      *this\cards[w] = *this\cards[i]
    EndIf

  Next

  For i = 0 To *this\size
    *this\cards[i] = shuffled(i)
  Next
EndProcedure

Procedure newDeck()
  Protected *newDeck._membersDeckClass = AllocateMemory(SizeOf(_membersDeckClass))
  If *newDeck
    *newDeck\vtable = ?vTable_deckClass
    *newDeck\size = #MaxCards - 1
    setInitialValues(*newDeck)
  EndIf
  ProcedureReturn *newDeck
EndProcedure

DataSection
  vTable_deckClass:
  Data.i @setInitialValues()
  Data.i @shuffle()
  Data.i @dealCard()
  Data.i @showDeck()

  cardDat:
  Data.s "2D", "3D", "4D", "5D", "6D", "7D", "8D", "9D", "TD", "JD", "QD", "KD", "AD"
  Data.s "2C", "3C", "4C", "5C", "6C", "7C", "8C", "9C", "TC", "JC", "QC", "KC", "AC"
  Data.s "2H", "3H", "4H", "5H", "6H", "7H", "8H", "9H", "TH", "JH", "QH", "KH", "AH"
  Data.s "2S", "3S", "4S", "5S", "6S", "7S", "8S", "9S", "TS", "JS", "QS", "KS", "AS"
EndDataSection

If OpenConsole()

  Define deck.deckObject = newDeck()
  Define deck2.deckObject = newDeck()

  If deck = 0 Or deck2 = 0
    PrintN("Unable to create decks")
    End
  EndIf

  deck\shuffle()
  PrintN("Dealt: " + deck\deal(#False))
  PrintN("Dealt: " + deck\deal(#False))
  PrintN("Dealt: " + deck\deal(#False))
  PrintN("Dealt: " + deck\deal(#False))
  deck\show()
  deck2\show()

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit")
  Input()
  CloseConsole()
EndIf
