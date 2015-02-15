-module( balanced_brackets ).
-export( [generate/1, is_balanced/1, task/0] ).

generate( N ) ->
	[generate_bracket(random:uniform()) || _X <- lists:seq(1, 2*N)].

is_balanced( String ) -> is_balanced_loop( String, 0 ).

task() ->
	lists:foreach( fun (N) ->
			String = generate( N ),
			Result = is_balanced( String ),
			io:fwrite( "~s is ~s~n", [String, task_balanced(Result)] )
		end,
		lists:seq(0, 5) ).



is_balanced_loop( _String, N ) when N < 0 -> false;
is_balanced_loop( [], 0 ) -> true;
is_balanced_loop( [], _N ) -> false;
is_balanced_loop( [$[ | T], N ) -> is_balanced_loop( T, N + 1 );
is_balanced_loop( [$] | T], N ) -> is_balanced_loop( T, N - 1 ).

generate_bracket( N ) when N =< 0.5 -> $[;
generate_bracket( N ) when N > 0.5 -> $].

task_balanced( true ) -> "OK";
task_balanced( false ) -> "NOT OK".
