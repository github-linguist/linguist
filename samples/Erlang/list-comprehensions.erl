pythag(N) ->
    [ {A,B,C} || A <- lists:seq(1,N),
                 B <- lists:seq(A,N),
                 C <- lists:seq(B,N),
                 A+B+C =< N,
                 A*A+B*B == C*C ].
