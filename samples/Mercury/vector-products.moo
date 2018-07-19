:- module vector_product.
:- interface.

:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module int, list, string.

main(!IO) :-
    A = vector3d(3, 4, 5),
    B = vector3d(4, 3, 5),
    C = vector3d(-5, -12, -13),
    io.format("A . B = %d\n", [i(A `dot_product` B)], !IO),
    io.format("A x B = %s\n", [s(to_string(A `cross_product` B))], !IO),
    io.format("A . (B x C) = %d\n", [i(scalar_triple_product(A, B, C))], !IO),
    io.format("A x (B x C) = %s\n", [s(to_string(vector_triple_product(A, B, C)))], !IO).

:- type vector3d ---> vector3d(int, int, int).

:- func dot_product(vector3d, vector3d) = int.

dot_product(vector3d(A1, A2, A3), vector3d(B1, B2, B3)) =
    A1 * B1 + A2 * B2 + A3 * B3.

:- func cross_product(vector3d, vector3d) = vector3d.

cross_product(vector3d(A1, A2, A3), vector3d(B1, B2, B3)) =
    vector3d(A2 * B3 - A3 * B2, A3 * B1 - A1 * B3, A1 * B2 - A2 * B1).

:- func scalar_triple_product(vector3d, vector3d, vector3d) = int.

scalar_triple_product(A, B, C) = A `dot_product` (B `cross_product` C).

:- func vector_triple_product(vector3d, vector3d, vector3d) = vector3d.

vector_triple_product(A, B, C) = A `cross_product` (B `cross_product` C).

:- func to_string(vector3d) = string.

to_string(vector3d(X, Y, Z)) =
    string.format("(%d, %d, %d)", [i(X), i(Y), i(Z)]).
