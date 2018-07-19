-module( exceptions ).

-export( [task/0] ).

task() ->
    try
    erlang:throw( new_exception )

    catch
    _:Exception -> io:fwrite( "Catched ~p~n", [Exception] )

    end.
