continued_fraction :-
	% square root 2
	continued_fraction(200, sqrt_2_ab, V1),
	format('sqrt(2) = ~w~n', [V1]),

	% napier
	continued_fraction(200, napier_ab, V2),
	format('e       = ~w~n', [V2]),

	% pi
	continued_fraction(200, pi_ab, V3),
	format('pi      = ~w~n', [V3]).


% code for continued fractions
continued_fraction(N, Compute_ab, V) :-
	continued_fraction(N,  Compute_ab, 0, V).

continued_fraction(0,  Compute_ab, Temp, V) :-
	call(Compute_ab, 0, A, _),
	V is A + Temp.

continued_fraction(N, Compute_ab, Tmp, V) :-
	call(Compute_ab, N, A, B),
	Tmp1 is B / (A + Tmp),
	N1 is N - 1,
	continued_fraction(N1, Compute_ab, Tmp1, V).

% specific codes for examples
% definitions for square root of 2
sqrt_2_ab(0, 1, 1).
sqrt_2_ab(_, 2, 1).

% definitions for napier
napier_ab(0, 2, _).
napier_ab(1, 1, 1).
napier_ab(N, N, V) :-
	V is N - 1.

% definitions for pi
pi_ab(0, 3, _).
pi_ab(N, 6, V) :-
	V is (2 * N - 1)*(2 * N - 1).
