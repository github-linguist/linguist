-module( function_frequency ).

-export( [erlang_source/1, task/0] ).

erlang_source( File ) ->
    {ok, IO} = file:open( File, [read] ),
    Forms = parse_all( IO, io:parse_erl_form(IO, ''), [] ),
    Functions = lists:flatten( [erl_syntax_lib:fold(fun accumulate_functions/2, [], X) || X <- Forms] ),
    dict:to_list( lists:foldl(fun count/2, dict:new(), Functions) ).

task() ->
    Function_frequencies = erlang_source( "function_frequency.erl" ),
    {Top_tens, _Rest} = lists:split( 10, lists:reverse(lists:keysort(2, Function_frequencies)) ),
    [io:fwrite("Function ~p called ~p times.~n", [X, Y]) || {X, Y} <- Top_tens].



accumulate_functions( Tree, Acc ) -> accumulate_functions( erlang:element(1, Tree), Tree, Acc ).

accumulate_functions( call, Tree, Acc ) -> [accumulate_functions_name(Tree) | Acc];
accumulate_functions( _Other, _Tree, Acc ) -> Acc.

accumulate_functions_name( Tree ) -> accumulate_functions_name_scoop( erlang:element(3, Tree) ).

accumulate_functions_name_scoop( {atom, _Line, Name} ) -> Name;
accumulate_functions_name_scoop( {remote, _Line, {atom, _Line, Module}, {atom, _Line, Name}} ) -> {Module, Name}.

count( Key, Dict ) -> dict:update_counter( Key, 1, Dict ).

parse_all( _IO, {eof, _End}, Acc ) -> Acc;
parse_all( IO, {ok, Tokens, Location}, Acc ) -> parse_all( IO, io:parse_erl_form(IO, '', Location), [Tokens | Acc] ).
