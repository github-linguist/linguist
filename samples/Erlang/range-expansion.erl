-module( range ).

-export( [expansion/1, task/0] ).

expansion( String ) ->
        lists:flatten( [expansion_individual(io_lib:fread("~d", X)) || X <- string:tokens(String, ",")] ).

task() ->
    io:fwrite( "~p~n", [expansion("-6,-3--1,3-5,7-11,14,15,17-20")] ).



expansion_individual( {ok, [N], []} ) -> N;
expansion_individual( {ok, [Start], "-" ++ Stop_string} ) -> lists:seq( Start, erlang:list_to_integer(Stop_string) ).
