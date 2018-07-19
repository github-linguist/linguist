% initial condition
do(0):- write(0),nl,do(1).

% control condition
do(V):- 0 is mod(V,6), !, fail.

% loop
do(V) :-
    write(V),nl,
    Y is V + 1,
    do(Y).

wloop :-
   do(0).
