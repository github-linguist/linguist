-module( roman_numerals ).

-export( [decode_from_string/1]).

to_value($M) -> 1000;
to_value($D) ->  500;
to_value($C) ->  100;
to_value($L) ->   50;
to_value($X) ->   10;
to_value($V) ->    5;
to_value($I) ->    1.

decode_from_string([]) -> 0;
decode_from_string([H1]) -> to_value(H1);
decode_from_string([H1, H2 |Rest]) ->
    case {to_value(H1), to_value(H2)} of
        {V1, V2} when V1 < V2 -> V2 - V1 + decode_from_string(Rest);
        {V1, V1} -> V1 + V1 + decode_from_string(Rest);
        {V1, _} -> V1 + decode_from_string([H2|Rest])
    end.
