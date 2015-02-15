-module( zigzag ).

-export( [matrix/1, task/0] ).

matrix( N ) ->
	{{_X_Y, N}, Proplist} = lists:foldl( fun matrix_as_proplist/2, {{{0, 0}, N}, []}, lists:seq(0, (N * N) - 1) ),
	[columns( X, Proplist ) || X <- lists:seq(0, N - 1)].

task() -> matrix( 5 ).



columns( Column, Proplist ) -> lists:sort( [Value || {{_X, Y}, Value} <- Proplist, Y =:= Column] ).

matrix_as_proplist( N, {{X_Y, Max}, Acc} ) ->
	Next = next_indexes( X_Y, Max ),
	{{Next, Max}, [{X_Y, N} | Acc]}.

next_indexes( {X, Y}, Max ) when Y + 1 =:= Max, (X + Y) rem 2 =:= 0  -> {X + 1, Y - 1};
next_indexes( {X, Y}, Max ) when Y + 1 =:= Max, (X + Y) rem 2 =:= 1  -> {X + 1, Y};
next_indexes( {X, Y}, Max ) when X + 1 =:= Max, (X + Y) rem 2 =:= 0  -> {X, Y + 1};
next_indexes( {X, Y}, Max ) when X + 1 =:= Max, (X + Y) rem 2 =:= 1  -> {X - 1, Y + 1};
next_indexes( {X, 0}, _Max ) when X rem 2 =:= 0 -> {X + 1, 0};
next_indexes( {X, 0}, _Max ) when X rem 2 =:= 1 -> {X - 1, 1};
next_indexes( {0, Y}, _Max ) when Y rem 2 =:= 0 -> {1, Y - 1};
next_indexes( {0, Y}, _Max ) when Y rem 2 =:= 1 -> {0, Y + 1};
next_indexes( {X, Y}, _Max ) when (X + Y) rem 2 =:= 0 -> {X + 1, Y - 1};
next_indexes( {X, Y}, _Max ) when (X + Y) rem 2 =:= 1 -> {X - 1, Y + 1}.
