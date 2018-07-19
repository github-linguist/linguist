play24(Len, Range, Goal) :-
	game(Len, Range, Goal, L, S),
	maplist(my_write, L),
	format(': ~w~n', [S]).

game(Len, Range, Value, L, S) :-
	length(L, Len),
	maplist(choose(Range), L),
	compute(L, Value, [], S).


choose(Range, V) :-
	V is random(Range) + 1.


write_tree([M], [M]).

write_tree([+, M, N], S) :-
	write_tree(M, MS),
	write_tree(N, NS),
	append(MS, [+ | NS], S).

write_tree([-, M, N], S) :-
	write_tree(M, MS),
	write_tree(N, NS),
	(   is_add(N) -> append(MS, [-, '(' | NS], Temp), append(Temp, ')', S)
	;   append(MS, [- | NS], S)).


write_tree([Op, M, N], S) :-
	member(Op, [*, /]),
	write_tree(M, MS),
	write_tree(N, NS),
	(   is_add(M) -> append(['(' | MS], [')'], TempM)
	;  TempM = MS),
	(   is_add(N) -> append(['(' | NS], [')'], TempN)
	;   TempN = NS),
	append(TempM, [Op | TempN], S).

is_add([Op, _, _]) :-
	member(Op, [+, -]).

compute([Value], Value, [[_R-S1]], S) :-
	write_tree(S1, S2),
	with_output_to(atom(S), maplist(write, S2)).

compute(L, Value, CS, S) :-
	select(M, L, L1),
	select(N, L1, L2),
	next_value(M, N, R, CS, Expr),
	compute([R|L2], Value, Expr, S).

next_value(M, N, R, CS,[[R - [+, M1, N1]] | CS2]) :-
	R is M+N,
	(   member([M-ExprM], CS) -> select([M-ExprM], CS, CS1), M1 = ExprM
	;   M1 = [M], CS1 = CS
	),
	(   member([N-ExprN], CS1) -> select([N-ExprN], CS1, CS2), N1 = ExprN
	;   N1 = [N], CS2 = CS1
	).

next_value(M, N, R, CS,[[R - [-, M1, N1]] | CS2]) :-
	R is M-N,
	(   member([M-ExprM], CS) -> select([M-ExprM], CS, CS1), M1 = ExprM
	;   M1 = [M], CS1 = CS
	),
	(   member([N-ExprN], CS1) -> select([N-ExprN], CS1, CS2), N1 = ExprN
	;   N1 = [N], CS2 = CS1
	).

next_value(M, N, R, CS,[[R - [*, M1, N1]] | CS2]) :-
	R is M*N,
	(   member([M-ExprM], CS) -> select([M-ExprM], CS, CS1), M1 = ExprM
	;   M1 = [M], CS1 = CS
	),
	(   member([N-ExprN], CS1) -> select([N-ExprN], CS1, CS2), N1 = ExprN
	;   N1 = [N], CS2 = CS1
	).

next_value(M, N, R, CS,[[R - [/, M1, N1]] | CS2]) :-
	N \= 0,
	R is rdiv(M,N),
	(   member([M-ExprM], CS) -> select([M-ExprM], CS, CS1), M1 = ExprM
	;   M1 = [M], CS1 = CS
	),
	(   member([N-ExprN], CS1) -> select([N-ExprN], CS1, CS2), N1 = ExprN
	;   N1 = [N], CS2 = CS1
	).

my_write(V) :-
	format('~w ', [V]).
