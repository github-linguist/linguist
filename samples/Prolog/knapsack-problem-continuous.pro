:- use_module(library(simplex)).
% tuples (name, weights, value).
knapsack :-
	L = [(   beef, 	  3.8, 	36),
	     (   pork, 	  5.4, 	43),
	     (   ham, 	  3.6, 	90),
	     (   greaves, 2.4, 	45),
	     (   flitch,  4.0, 	30),
	     (   brawn,   2.5, 	56),
	     (   welt, 	  3.7, 	67),
	     (   salami,  3.0, 	95),
	     (   sausage, 5.9, 	98)],

	 gen_state(S0),
	 length(L, N),
	 numlist(1, N, LN),
	 (   (  create_constraint_N(LN, L, S0, S1, [], LW, [], LV),
		constraint(LW =< 15.0, S1, S2),
		maximize(LV, S2, S3)
	      )),
	compute_lenword(L, 0, Len),
	sformat(A1, '~~w~~t~~~w|', [Len]),
	sformat(A2, '~~t~~2f~~~w|', [10]),
	sformat(A3, '~~t~~2f~~~w|', [10]),
	print_results(S3, A1,A2,A3, L, LN, 0, 0).


create_constraint_N([], [], S, S, LW, LW, LV, LV).

create_constraint_N([HN|TN], [(_, W, V) | TL], S1, SF, LW, LWF, LV, LVF) :-
	constraint([x(HN)] >= 0, S1, S2),
	constraint([x(HN)] =< W, S2, S3),
	X is V/W,
	create_constraint_N(TN, TL, S3, SF, [x(HN) | LW], LWF, [X * x(HN) | LV], LVF).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
compute_lenword([], N, N).
compute_lenword([(Name, _, _)|T], N, NF):-
	atom_length(Name, L),
	(   L > N -> N1 = L; N1 = N),
	compute_lenword(T, N1, NF).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
print_results(_S, A1, A2, A3, [], [], WM, VM) :-
	sformat(W1, A1, [' ']),
	sformat(W2, A2, [WM]),
	sformat(W3, A3, [VM]),
	format('~w~w~w~n', [W1,W2,W3]).


print_results(S, A1, A2, A3, [(Name, W, V)|T], [N|TN], W1, V1) :-
	variable_value(S, x(N), X),
	(   X = 0 -> W1 = W2, V1 = V2
	;
	    sformat(S1, A1, [Name]),
	    sformat(S2, A2, [X]),
	    Vtemp is X * V/W,
	    sformat(S3, A3, [Vtemp]),
	    format('~w~w~w~n', [S1,S2,S3]),
	    W2 is W1 + X,
	    V2 is V1 + Vtemp ),
	print_results(S, A1, A2, A3, T, TN, W2, V2).
