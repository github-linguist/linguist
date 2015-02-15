-module( loops_nested ).

-export( [task/0] ).

task() ->
       Size = 20,
       Two_dimensional_array = [random_array(Size) || _X <- lists:seq(1, Size)],
       print_until_found( [], 20, Two_dimensional_array ).



print_until_found( [], N, [Row | T] ) -> print_until_found( print_until_found_row(N, Row), N, T );
print_until_found( _Found, _N, _Two_dimensional_array ) -> io:fwrite( "~n" ).

print_until_found_row( _N, [] ) -> [];
print_until_found_row( N, [N | T] ) -> [N | T];
print_until_found_row( N, [H | T] ) ->
        io:fwrite( "~p ", [H] ),
        print_until_found_row( N, T ).

random_array( Size ) -> [random:uniform(Size) || _X <- lists:seq(1, Size)].
