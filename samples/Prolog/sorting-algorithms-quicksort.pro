qsort( [], [] ).
qsort( [H|U], S ) :- splitBy(H, U, L, R), qsort(L, SL), qsort(R, SR), append(SL, [H|SR], S).

% splitBy( H, U, LS, RS )
% True if LS = { L in U | L <= H }; RS = { R in U | R > H }
splitBy( _, [], [], []).
splitBy( H, [U|T], [U|LS], RS ) :- U =< H, splitBy(H, T, LS, RS).
splitBy( H, [U|T], LS, [U|RS] ) :- U  > H, splitBy(H, T, LS, RS).
