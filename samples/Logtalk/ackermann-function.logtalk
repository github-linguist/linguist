ack(0, N, V) :-
    !,
    V is N + 1.
ack(M, 0, V) :-
    !,
    M2 is M - 1,
    ack(M2, 1, V).
ack(M, N, V) :-
    M2 is M - 1,
    N2 is N - 1,
    ack(M, N2, V2),
    ack(M2, V2, V).
