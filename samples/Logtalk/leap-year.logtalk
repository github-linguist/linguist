leap_year(Year) :-
    (   mod(Year, 4) =:= 0, mod(Year, 100) =\= 0 ->
        true
    ;   mod(Year, 400) =:= 0
    ).
