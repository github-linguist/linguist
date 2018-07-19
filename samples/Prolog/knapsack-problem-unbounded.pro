:- use_module(library(simplex)).

% tuples (name, Explantion, Value, weights, volume).
knapsack :-
	L =[(	panacea, 'Incredible healing properties', 3000,	0.3,	0.025),
	    (	ichor,   'Vampires blood',                1800,	0.2,	0.015),
	    (	gold ,	 'Shiney shiney',	          2500,	2.0,	0.002)],

	 gen_state(S0),
	 length(L, N),
	 numlist(1, N, LN),

	 % to get statistics
	 time((create_constraint_N(LN, L, S0, S1, [], LVa, [], LW, [], LVo),
	       constraint(LW =< 25.0, S1, S2),
	       constraint(LVo =< 0.25, S2, S3),
	       maximize(LVa, S3, S4)
	      )),

	% we display the results
	compute_lenword(L, 0, Len),
	sformat(A0, '~~w~~t~~~w|', [3]),
	sformat(A1, '~~w~~t~~~w|', [Len]),
	sformat(A2, '~~t~~w~~~w|', [10]),
	sformat(A3, '~~t~~2f~~~w|', [10]),
	sformat(A4, '~~t~~3f~~~w|', [10]),
	sformat(A33, '~~t~~w~~~w|', [10]),
	sformat(A44, '~~t~~w~~~w|', [10]),

	sformat(W0, A0, ['Nb']),
	sformat(W1, A1, ['Items']),
	sformat(W2, A2, ['Value']),
	sformat(W3, A33, ['Weigth']),
	sformat(W4, A44, ['Volume']),
	format('~w~w~w~w~w~n', [W0, W1,W2,W3,W4]),

	print_results(S4, A0, A1, A2, A3, A4, L, LN, 0, 0, 0).


create_constraint_N([], [], S, S, LVa, LVa, LW, LW, LVo, LVo).

create_constraint_N([HN|TN], [(_, _,Va, W, Vo) | TL], S1, SF, LVa, LVaF, LW, LWF, LVo, LVoF) :-
	constraint(integral(x(HN)), S1, S2),
	constraint([x(HN)] >= 0, S2, S3),
	create_constraint_N(TN, TL, S3, SF,
			    [Va * x(HN) | LVa], LVaF,
			    [W * x(HN) | LW], LWF,
			    [Vo * x(HN) | LVo], LVoF).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
compute_lenword([], N, N).
compute_lenword([(Name, _, _, _, _)|T], N, NF):-
	atom_length(Name, L),
	(   L > N -> N1 = L; N1 = N),
	compute_lenword(T, N1, NF).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
print_results(_S, A0, A1, A2, A3, A4, [], [], VaM, WM, VoM) :-
	sformat(W0, A0, [' ']),
	sformat(W1, A1, [' ']),
	sformat(W2, A2, [VaM]),
	sformat(W3, A3, [WM]),
	sformat(W4, A4, [VoM]),
	format('~w~w~w~w~w~n', [W0, W1,W2,W3,W4]).


print_results(S, A0, A1, A2, A3, A4, [(Name, _, Va, W, Vo)|T], [N|TN], Va1, W1, Vo1) :-
	variable_value(S, x(N), X),
	(   X = 0 -> Va1 = Va2, W1 = W2, Vo1 = Vo2
	;
	    sformat(S0, A0, [X]),
	    sformat(S1, A1, [Name]),
	    Vatemp is X * Va,
	    Wtemp is X * W,
	    Votemp is X * Vo,
	    sformat(S2, A2, [Vatemp]),
	    sformat(S3, A3, [Wtemp]),
	    sformat(S4, A4, [Votemp]),
	    format('~w~w~w~w~w~n', [S0,S1,S2,S3,S4]),
	    Va2 is Va1 + Vatemp,
	    W2 is W1 + Wtemp,
	    Vo2 is Vo1 + Votemp ),
	print_results(S, A0, A1, A2, A3, A4, T, TN, Va2, W2, Vo2).
