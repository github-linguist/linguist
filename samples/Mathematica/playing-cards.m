MakeDeck[] :=  Tuples[{{"Ace ", 2, 3 , 4 , 5, 6 , 7 , 8 , 9 , 10, "Jack" , "Queen", "King"}, {♦ , ♣, ♥ , ♠}}]
DeckShuffle[deck_] := RandomSample[deck, Length@deck]
DealFromDeck[] := (Print@First@deck; deck = deck[[2 ;; All]];)
