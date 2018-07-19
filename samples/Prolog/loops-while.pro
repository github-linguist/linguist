while(0) :- !.
while(X) :- X>0,write(X), nl, X1 is X // 2, while(X1).
