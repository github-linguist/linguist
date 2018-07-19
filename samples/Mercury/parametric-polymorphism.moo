:- type tree(A) ---> empty ; node(A, tree(A), tree(A)).

:- func map(func(A) = B, tree(A)) = tree(B).

map(_, empty) = empty.
map(F, node(A, Left, Right)) = node(F(A), map(F, Left), map(F, Right)).
