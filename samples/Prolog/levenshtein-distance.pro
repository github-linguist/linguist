levenshtein(S, T, R) :-
	length(S, M),
	M1 is M+1,
	length(T, N),
	N1 is N+1,
	length(Lev, N1),
	maplist(init(M1), Lev),
	numlist(0, N, LN),
	maplist(init_n, LN, Lev),
	nth0(0, Lev, Lev0),
	numlist(0, M, Lev0),

	% compute_levenshtein distance
	numlist(1, N, LN1),
	maplist(work_on_T(Lev, S), LN1, T),
	last(Lev, LevLast),
	last(LevLast, R).


work_on_T(Lev, S, J, TJ) :-
	length(S, M),
	numlist(1, M, LM),
	maplist(work_on_S(Lev, J, TJ), LM, S).

work_on_S(Lev, J, C, I, C) :-
	% same char
	!,
	I1 is I-1, J1 is J-1,
	nth0(J1, Lev, LevJ1),
	nth0(I1, LevJ1, V),
	nth0(J, Lev, LevJ),
	nth0(I, LevJ, V).


work_on_S(Lev, J, _C1, I, _C2) :-
	I1 is I-1, J1 is J - 1,
	% compute the value for deletion
	nth0(J, Lev, LevJ),
	nth0(I1, LevJ, VD0),
	VD is VD0 + 1,

	% compute the value for insertion
	nth0(J1, Lev, LevJ1),
	nth0(I, LevJ1, VI0),
	VI is VI0 + 1,

	% compute the value for substitution
	nth0(I1, LevJ1, VS0),
	VS is VS0 + 1,

	% set the minimum value to cell(I,J)
	sort([VD, VI, VS], [V|_]),

	nth0(I, LevJ, V).


init(Len, C) :-
	length(C, Len).

init_n(N, L) :-
	nth0(0, L, N).
