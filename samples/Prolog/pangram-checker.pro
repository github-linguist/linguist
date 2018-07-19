pangram(L) :-
	numlist(0'a, 0'z, Alphabet),
	forall(member(C, Alphabet), member(C, L)).

pangram_example :-
	L1 = "the quick brown fox jumps over the lazy dog",
	(   pangram(L1) -> R1= ok; R1 = ko),
	format('~s --> ~w ~n', [L1,R1]),

	L2 = "the quick brown fox jumped over the lazy dog",
	(   pangram(L2) -> R2 = ok; R2 = ko),
	format('~s --> ~w ~n', [L2, R2]).
