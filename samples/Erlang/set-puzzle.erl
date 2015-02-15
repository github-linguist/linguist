-module( set ).

-export( [deck/0, is_set/3, shuffle_deck/1, task/0] ).

-record( card, {number, symbol, shading, colour} ).

deck() -> [#card{number=N, symbol=Sy, shading=Sh, colour=C} || N <- [1,2,3], Sy <- [diamond, squiggle, oval], Sh <- [solid, striped, open], C <- [red, green, purple]].

is_set( Card1, Card2, Card3 ) ->
        is_colour_correct( Card1, Card2, Card3 )
        andalso is_number_correct( Card1, Card2, Card3 )
        andalso is_shading_correct( Card1, Card2, Card3 )
        andalso is_symbol_correct( Card1, Card2, Card3 ).

shuffle_deck( Deck ) -> knuth_shuffle:list( Deck ).

task() ->
    basic(),
    advanced().



advanced() -> common( 6, 12 ).

basic() -> common( 4, 9 ).

common(	X, Y ) ->
    {Sets, Cards} = find_x_sets_in_y_cards( X, Y, deck() ),
    io:fwrite( "Cards ~p~n", [Cards] ),
    io:fwrite( "Gives sets:~n" ),
    [io:fwrite( "~p~n", [S] ) || S <- Sets].

find_x_sets_in_y_cards( X, Y, Deck ) ->
    {Cards, _T} = lists:split( Y, shuffle_deck(Deck) ),
    find_x_sets_in_y_cards( X, Y, Cards, make_sets1(Cards, []) ).

find_x_sets_in_y_cards( X, _Y, _Deck, Cards, Sets ) when erlang:length(Sets) =:= X -> {Sets, Cards};
find_x_sets_in_y_cards( X, Y, Deck, _Cards, _Sets ) -> find_x_sets_in_y_cards( X, Y, Deck ).

is_colour_correct( Card1, Card2, Card3 ) -> is_colour_different( Card1, Card2, Card3 ) orelse is_colour_same( Card1, Card2, Card3 ).

is_colour_different( #card{colour=C1}, #card{colour=C2}, #card{colour=C3} ) when C1 =/= C2, C1 =/= C3, C2 =/= C3 -> true;
is_colour_different( _Card1, _Card2, _Card3 ) -> false.

is_colour_same( #card{colour=C}, #card{colour=C}, #card{colour=C} ) -> true;
is_colour_same( _Card1, _Card2, _Card3 ) -> false.

is_number_correct( Card1, Card2, Card3 ) -> is_number_different( Card1, Card2, Card3 ) orelse is_number_same( Card1, Card2, Card3 ).

is_number_different( #card{number=N1}, #card{number=N2}, #card{number=N3} ) when N1 =/= N2, N1 =/= N3, N2 =/= N3 -> true;
is_number_different( _Card1, _Card2, _Card3 ) -> false.

is_number_same( #card{number=N}, #card{number=N}, #card{number=N} ) -> true;
is_number_same( _Card1, _Card2, _Card3 ) -> false.

is_shading_correct( Card1, Card2, Card3 ) -> is_shading_different( Card1, Card2, Card3 ) orelse is_shading_same( Card1, Card2, Card3 ).

is_shading_different( #card{shading=S1}, #card{shading=S2}, #card{shading=S3} ) when S1 =/= S2, S1 =/= S3, S2 =/= S3 -> true;
is_shading_different( _Card1, _Card2, _Card3 ) -> false.

is_shading_same( #card{shading=S}, #card{shading=S}, #card{shading=S} ) -> true;
is_shading_same( _Card1, _Card2, _Card3 ) -> false.

is_symbol_correct( Card1, Card2, Card3 ) -> is_symbol_different( Card1, Card2, Card3 ) orelse is_symbol_same( Card1, Card2, Card3 ).

is_symbol_different( #card{symbol=S1}, #card{symbol=S2}, #card{symbol=S3} ) when S1 =/= S2, S1 =/= S3, S2 =/= S3 -> true;
is_symbol_different( _Card1, _Card2, _Card3 ) -> false.

is_symbol_same( #card{symbol=S}, #card{symbol=S}, #card{symbol=S} ) -> true;
is_symbol_same( _Card1, _Card2, _Card3 ) -> false.
%% Nested loops 1, 2 and 3
make_sets1( [_Second_to_last, _Last], Sets ) -> Sets;
make_sets1( [Card | T], Sets ) -> make_sets1( T, make_sets2(Card, T, Sets) ).

make_sets2( _Card, [_Last], Sets ) -> Sets;
make_sets2( Card1, [Card2 | T], Sets ) -> make_sets2( Card1, T, make_sets3( Card1, Card2, T,  Sets) ).

make_sets3( _Card1, _Card2, [], Sets ) -> Sets;
make_sets3( Card1, Card2, [Card3 | T], Sets ) ->
        make_sets3( Card1, Card2, T, make_sets_acc(is_set(Card1, Card2, Card3), {Card1, Card2, Card3}, Sets) ).

make_sets_acc( true, Set, Sets ) -> [Set | Sets];
make_sets_acc( false, _Set, Sets ) -> Sets.
