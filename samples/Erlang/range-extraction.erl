-module( range ).

-export( [extraction/1, task/0] ).

extraction( [H | T] ) when is_integer(H) ->
        Reversed_extracts = extraction_acc( lists:foldl(fun extraction/2, {H, []}, T) ),
        string:join( lists:reverse(Reversed_extracts), "," ).

task() ->
    io:fwrite( "~p~n", [extraction([0, 1, 2, 4, 6, 7, 8, 11, 12, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 27, 28, 29, 30, 31, 32, 33, 35, 36, 37, 38, 39])] ).



extraction( N, {Start, Acc} ) when N =:= Start + 1 -> {Start, N, Acc};
extraction( N, {Start, Acc} )  -> {N, extraction_acc( {Start, Acc} )};
extraction( N, {Start, Stop, Acc} ) when N =:= Stop + 1 -> {Start, N, Acc};
extraction( N, {Start, Stop, Acc} ) -> {N, extraction_acc( {Start, Stop, Acc} )}.

extraction_acc( {N, Acc} ) -> [erlang:integer_to_list(N) | Acc];
extraction_acc( {Start, Stop, Acc} ) when Stop > Start + 1 -> [erlang:integer_to_list(Start) ++ "-" ++ erlang:integer_to_list(Stop) | Acc];
extraction_acc( {Start, Stop, Acc} ) -> [erlang:integer_to_list(Stop), erlang:integer_to_list(Start) | Acc]. % Reversed
