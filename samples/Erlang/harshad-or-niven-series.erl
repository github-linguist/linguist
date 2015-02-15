-module( harshad ).

-export( [greater_than/1, sequence/1, task/0] ).

greater_than( N ) when N >= 1  ->
        greater_than( 2, N, acc(1, {0, []}) ).

sequence( Find_this_many ) when Find_this_many >= 1 ->
        sequence( 2, Find_this_many, acc(1, {0, []}) ).

task() ->
        io:fwrite( "~p~n", [sequence(20)] ),
        io:fwrite( "~p~n", [greater_than(1000)] ).



acc( N, Acc ) -> acc( N rem lists:sum([X - $0|| X <- erlang:integer_to_list(N)]), N, Acc ).

acc( 0, N, {Found, Acc} ) -> {Found + 1, [N | Acc]};
acc( _Reminder, _N, Acc ) -> Acc.

greater_than( _N, Find, {_, [Found | _T]} ) when Found > Find -> Found;
greater_than( N, Find, Acc ) ->	greater_than( N + 1, Find, acc(N, Acc) ).

sequence( _N, Found, {Found, Acc} ) -> lists:reverse( Acc );
sequence( N, Find, Acc ) -> sequence( N + 1, Find, acc(N, Acc) ).
