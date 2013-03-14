%%----- normalize(+Wff,-NormalClauses) ------
normalize(Wff,NormalClauses) :-
   conVert(Wff,[],S),
   cnF(S,T),
   flatten_and(T,U),
   make_clauses(U,NormalClauses).

%%-----  make a sequence out of a conjunction -----
flatten_and(X /\ Y, F) :-
   !,
   flatten_and(X,A),
   flatten_and(Y, B),
   sequence_append(A,B,F).
flatten_and(X,X).

%%-----  make a sequence out of a disjunction -----
flatten_or(X \/ Y, F) :-
   !,
   flatten_or(X,A),
   flatten_or(Y,B),
   sequence_append(A,B,F).
flatten_or(X,X).


%%----- append two sequences -------------------------------
sequence_append((X,R),S,(X,T)) :- !, sequence_append(R,S,T).
sequence_append((X),S,(X,S)).

%%----- separate into positive and negative literals -----------
separate((A,B),P,N) :-
   !,
   (A = ~X -> N=[X|N1],
               separate(B,P,N1)
             ;
               P=[A|P1],
               separate(B,P1,N) ).
separate(A,P,N) :-
   (A = ~X -> N=[X],
               P = []
            ;
               P=[A],
               N = [] ).

%%----- tautology ----------------------------
tautology(P,N) :- some_occurs(N,P).

some_occurs([F|R],B) :-
   occurs(F,B) | some_occurs(R,B).

occurs(A,[F|_]) :-
   A == F,
   !.
occurs(A,[_|R]) :-
   occurs(A,R).

make_clauses((A,B),C) :-
   !,
   flatten_or(A,F),
   separate(F,P,N),
   (tautology(P,N) ->
      make_clauses(B,C)
          ;
      make_clause(P,N,D),
      C = [D|R],
      make_clauses(B,R) ).
make_clauses(A,C) :-
   flatten_or(A,F),
   separate(F,P,N),
   (tautology(P,N) ->
       C = []
        ;
       make_clause(P,N,D),
       C = [D] ).

make_clause([],N, false :- B) :-
   !,
   make_sequence(N,B,',').
make_clause(P,[],H) :-
   !,
   make_sequence(P,H,'|').
make_clause(P,N, H :- T) :-
   make_sequence(P,H,'|'),
   make_sequence(N,T,',').

make_sequence([A],A,_) :- !.
make_sequence([F|R],(F|S),'|') :-
   make_sequence(R,S,'|').
make_sequence([F|R],(F,S),',') :-
   make_sequence(R,S,',').

write_list([F|R]) :-
   write(F), write('.'), nl,
   write_list(R).
write_list([]).
