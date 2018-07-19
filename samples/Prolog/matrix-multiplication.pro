% SWI-Prolog has transpose/2 in its clpfd library
:- use_module(library(clpfd)).

% N is the dot product of lists V1 and V2.
dot(V1, V2, N) :- maplist(product,V1,V2,P), sumlist(P,N).
product(N1,N2,N3) :- N3 is N1*N2.

% Matrix multiplication with matrices represented
% as lists of lists. M3 is the product of M1 and M2
mmult(M1, M2, M3) :- transpose(M2,MT), maplist(mm_helper(MT), M1, M3).
mm_helper(M2, I1, M3) :- maplist(dot(I1), M2, M3).
