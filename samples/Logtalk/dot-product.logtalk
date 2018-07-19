dot_product(A, B, Sum) :-
    dot_product(A, B, 0, Sum).

dot_product([], [], Sum, Sum).
dot_product([A| As], [B| Bs], Acc, Sum) :-
    Acc2 is Acc + A*B,
    dot_product(As, Bs, Acc2, Sum).
