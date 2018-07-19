:- module merge_sort.

:- interface.

:- import_module list.

:- type split_error ---> split_error.

:- func merge_sort(list(T)) = list(T).
:- pred merge_sort(list(T)::in, list(T)::out) is det.

:- implementation.

:- import_module int, exception.

merge_sort(U) = S :- merge_sort(U, S).

merge_sort(U, S) :- merge_sort(list.length(U), U, S).

:- pred merge_sort(int::in, list(T)::in, list(T)::out) is det.
merge_sort(L, U, S) :-
    ( L > 1 ->
        H = L // 2,
        ( split(H, U, F, B) ->
            merge_sort(H, F, SF),
            merge_sort(L - H, B, SB),
            merge_sort.merge(SF, SB, S)
        ; throw(split_error) )
    ; S = U ).

:- pred split(int::in, list(T)::in, list(T)::out, list(T)::out) is semidet.
split(N, L, S, E) :-
    ( N = 0 -> S = [], E = L
    ; N > 0, L = [H | L1], S = [H | S1],
      split(N - 1, L1, S1, E) ).

:- pred merge(list(T)::in, list(T)::in, list(T)::out) is det.
merge([], [], []).
merge([X|Xs], [], [X|Xs]).
merge([], [Y|Ys], [Y|Ys]).
merge([X|Xs], [Y|Ys], M) :-
    ( compare(>, X, Y) ->
        merge_sort.merge([X|Xs], Ys, M0),
        M = [Y|M0]
    ; merge_sort.merge(Xs, [Y|Ys], M0),
        M = [X|M0] ).
