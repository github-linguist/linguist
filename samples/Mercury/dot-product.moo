:- module dot_product.
:- interface.

:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module int, list.

main(!IO) :-
    io.write_int([1, 3, -5] `dot_product` [4, -2, -1], !IO),
    io.nl(!IO).

:- func dot_product(list(int), list(int)) = int.

dot_product(As, Bs) =
    list.foldl_corresponding((func(A, B, Acc) = Acc + A * B), As, Bs, 0).
