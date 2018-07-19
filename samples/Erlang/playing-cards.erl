-module( playing_cards ).

-export( [deal/2, deal/3, deck/0, print/1, shuffle/1, sort_pips/1, sort_suites/1, task/0] ).

-record( card, {pip, suite} ).

-spec( deal( N_cards::integer(), Deck::[#card{}]) -> {Hand::[#card{}], Deck::[#card{}]} ).
deal( N_cards, Deck ) -> lists:split( N_cards, Deck ).
-spec( deal( N_hands::integer(), N_cards::integer(), Deck::[#card{}]) -> {List_of_hands::[[#card{}]], Deck::[#card{}]} ).
deal( N_hands, N_cards, Deck ) -> lists:foldl( fun deal_hands/2, {lists:duplicate(N_hands, []), Deck}, lists:seq(1, N_cards * N_hands) ).

deck() -> [#card{suite=X, pip=Y} || X <- suites(), Y <- pips()].

print( Cards ) -> [io:fwrite( "	~p", [X]) || X <- Cards], io:nl().

shuffle( Deck ) -> knuth_shuffle:list( Deck ).

sort_pips( Cards ) -> lists:keysort( #card.pip, Cards ).

sort_suites( Cards ) -> lists:keysort( #card.suite, Cards ).

task() ->
    Deck = deck(),
    Shuffled = shuffle( Deck ),
    {Hand, New_deck} = deal( 3, Shuffled ),
    {Hands, _Deck} = deal( 2, 3, New_deck ),
    io:fwrite( "Hand:" ),
    print( Hand ),
    io:fwrite( "Hands:~n" ),
    [print(X) || X <- Hands].



deal_hands( _N, {[Hand | T], [Card | Deck]} ) -> {T ++ [[Card | Hand]], Deck}.

pips() -> ["2", "3", "4", "5", "6", "7", "8", "9", "10", "Jack", "Queen", "King", "Ace"].

suites() -> ["Clubs", "Hearts", "Spades", "Diamonds"].
