-module( configuration_file ).

-export( [read/1, task/0] ).

read( File ) ->
    {ok, Binary} = file:read_file( File ),
    Lines = [X || <<First:8, _T/binary>> = X <- binary:split(Binary, <<"\n">>, [global]), First =/= $#, First =/= $;],
    [option_from_binaries(binary:split(X, <<" ">>)) || X <- Lines].

task() ->
    Defaults = [{fullname, "Kalle"}, {favouritefruit, "apple"}, {needspeeling, false}, {seedsremoved, false}],
    Options = read( "configuration_file" ) ++ Defaults,
    [io:fwrite("~p = ~p~n", [X, proplists:get_value(X, Options)]) || X <- [fullname, favouritefruit, needspeeling, seedsremoved, otherfamily]].



option_from_binaries( [Option] ) -> {erlang:list_to_atom(string:to_lower(erlang:binary_to_list(Option))), true};
option_from_binaries( [Option, Values] ) -> {erlang:list_to_atom(string:to_lower(erlang:binary_to_list(Option))), option_from_binaries_value(binary:split(Values, <<", ">>))}.

option_from_binaries_value( [Value] ) -> erlang:binary_to_list(Value);
option_from_binaries_value( Values ) -> [erlang:binary_to_list(X) || X <- Values].
