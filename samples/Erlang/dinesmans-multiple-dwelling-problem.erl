-module( dinesman_multiple_dwelling ).

-export( [solve/2, task/0] ).

solve( All_persons, Rules ) ->
    [house(Bottom_floor, B, C, D, Top_floor) || Bottom_floor <- All_persons, B <- All_persons, C <- All_persons, D <- All_persons, Top_floor <- All_persons,
	lists:all( fun (Fun) ->	Fun( house(Bottom_floor, B, C, D, Top_floor) ) end, rules( Rules ))].

task() ->
    All_persons = [baker, cooper, fletcher, miller, smith],
    Rules = [all_on_different_floors, {not_lives_on_floor, 4, baker}, {not_lives_on_floor, 0, cooper}, {not_lives_on_floor, 4, fletcher}, {not_lives_on_floor, 0, fletcher},
          {on_higher_floor, miller, cooper}, {not_adjacent, smith, fletcher}, {not_adjacent, fletcher, cooper}],
    [House] = solve( All_persons, Rules ),
    [io:fwrite("~p lives on floor ~p~n", [lists:nth(X,	House),	X - 1]) || X <- lists:seq(1,5)].



house( A, B, C, D, E ) -> [A, B, C, D, E].

is_all_on_different_floors( [A, B, C, D, E] ) ->
        A =/= B andalso A =/= C andalso A =/= D andalso A =/= E
        andalso B =/= C andalso B =/= D andalso B =/= E
        andalso C =/= D andalso C =/= E
        andalso D =/= E.

is_not_adjacent( Person1, Person2, House ) ->
        is_not_below( Person1, Person2, House ) andalso is_not_below( Person2, Person1, House ).

is_not_below( _Person1, _Person2, [_Person] ) -> true;
is_not_below( Person1, Person2, [Person1, Person2 | _T] ) -> false;
is_not_below( Person1, Person2, [_Person | T] ) -> is_not_below( Person1, Person2, T ).

is_on_higher_floor( Person1, _Person2, [Person1 | _T] ) -> false;
is_on_higher_floor( _Person1, Person2, [Person2 | _T] ) -> true;
is_on_higher_floor( Person1, Person2, [_Person | T] ) -> is_on_higher_floor( Person1, Person2, T ).

rules( Rules ) -> lists:map( fun rules_fun/1, Rules ).

rules_fun( all_on_different_floors ) -> fun is_all_on_different_floors/1;
rules_fun( {not_lives_on_floor, N, Person} ) -> fun (House) -> Person =/= lists:nth(N + 1, House) end;
rules_fun( {on_higher_floor, Person1, Person2} ) -> fun (House) -> is_on_higher_floor( Person1, Person2, House ) end;
rules_fun( {not_below, Person1, Person2} ) -> fun (House) -> is_not_below( Person1, Person2, House ) end;
rules_fun( {not_adjacent, Person1, Person2} ) -> fun (House) ->	is_not_adjacent( Person1, Person2, House ) end.
