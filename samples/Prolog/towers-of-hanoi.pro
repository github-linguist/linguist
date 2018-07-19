hanoi(N) :- move(N,left,center,right).

move(0,_,_,_) :- !.
move(N,A,B,C) :-
    M is N-1,
    move(M,A,C,B),
    inform(A,B),
    move(M,C,B,A).

inform(X,Y) :- write([move,a,disk,from,the,X,pole,to,Y,pole]), nl.
