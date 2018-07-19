sum(S) :-
        findall(L, (between(1,1000,N),L is 1/N^2), Ls),
        sumlist(Ls, S).
