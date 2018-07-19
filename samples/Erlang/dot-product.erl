dotProduct(A,B) when length(A) == length(B) -> dotProduct(A,B,0);
dotProduct(_,_) -> erlang:error('Vectors must have the same length.').

dotProduct([H1|T1],[H2|T2],P) -> dotProduct(T1,T2,P+H1*H2);
dotProduct([],[],P) -> P.

dotProduct([1,3,-5],[4,-2,-1]).
