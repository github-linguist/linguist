-module( floyds_triangle ).

-export( [integers/1, print/1, strings/1, task/0] ).

integers( N ) ->
          lists:reverse( integers_reversed(N) ).

print( N ) ->
       [io:fwrite("~s~n", [lists:flatten(X)]) || X <- strings(N)].

strings( N ) ->
        Strings_reversed = [strings_from_integers(X) || X <- integers_reversed(N)],
        Paddings = paddings( [lengths(X) || X <- Strings_reversed] ),
        [formats(X, Y) || {X, Y} <- lists:zip(Paddings, lists:reverse(Strings_reversed))].

task() ->
       print( 5	),
       print( 14 ).



formats( Paddings, Strings ) -> [lists:flatten(io_lib:format(" ~*s", [X, Y])) || {X, Y} <- lists:zip(Paddings, Strings)].

integers_reversed( N ) ->
          {_End, Integers_reversed} = lists:foldl( fun integers_reversed/2, {1, []}, lists:seq(0, N - 1) ),
          Integers_reversed.

integers_reversed( N, {Start, Acc} ) ->
          End = Start + N,
          {End + 1, [lists:seq(Start, End) | Acc]}.

lengths( Strings ) -> [string:len(X) || X <- Strings].

paddings( [Last_line | T] ) ->
          {[], Paddings} = lists:foldl( fun paddings/2, {paddings_lose_last(Last_line), [Last_line]}, lists:seq(1, erlang:length(T)) ),
          Paddings.

paddings( _N, {Current,	Acc} ) -> {paddings_lose_last(Current),	[Current | Acc]}.

paddings_lose_last( List ) ->
	[_H | T]	= lists:reverse( List ),
	lists:reverse( T ).

strings_from_integers( Integers ) -> [erlang:integer_to_list(X) || X <- Integers].
