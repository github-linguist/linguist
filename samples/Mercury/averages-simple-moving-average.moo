    % state(period, list of floats from [newest, ..., oldest])
:- type state ---> state(int, list(float)).

:- func init(int) = state.
init(Period) = state(Period, []).

:- pred sma(float::in, float::out, state::in, state::out) is det.
sma(N, Average, state(P, L0), state(P, L)) :-
        take_upto(P, [N|L0], L),
        Average = foldl((+), L, 0.0) / float(length(L)).
