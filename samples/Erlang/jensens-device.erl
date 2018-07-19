-module( jensens_device ).

-export( [task/0] ).

task() ->
    sum( 1, 100, fun (I) -> 1 / I end ).



sum( I, High, _Term ) when I > High -> 0;
sum( I, High, Term ) ->
    Temp = Term( I ),
    Temp + sum( I + 1, High, Term ).
