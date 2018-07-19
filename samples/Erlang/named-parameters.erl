Fun = fun( Proplists ) ->
    Argument1 = proplists:get_value( argument1, Proplists, 1 ),
    Kalle = proplists:get_value( kalle, Proplists, "hobbe" ),
    io:fwrite( "~p ~s~n", [Argument1, Kalle] )
end.
