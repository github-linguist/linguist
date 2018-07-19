rosetta_brackets :-
	test_brackets([]),
	test_brackets(['[',']']),
	test_brackets(['[',']','[',']']),
	test_brackets(['[','[',']','[',']',']']),
	test_brackets([']','[']),
	test_brackets([']','[',']','[']),
	test_brackets(['[',']',']','[','[',']']).

balanced_brackets :-
	gen_bracket(2,  B1, []), test_brackets(B1),
	gen_bracket(4,  B2, []), test_brackets(B2),
	gen_bracket(4,  B3, []), test_brackets(B3),
	gen_bracket(6,  B4, []), test_brackets(B4),
	gen_bracket(6,  B5, []), test_brackets(B5),
	gen_bracket(8,  B6, []), test_brackets(B6),
	gen_bracket(8,  B7, []), test_brackets(B7),
	gen_bracket(10, B8, []), test_brackets(B8),
	gen_bracket(10, B9, []), test_brackets(B9).

test_brackets(Goal) :-
	(   Goal = [] -> write('(empty)'); maplist(write, Goal)),
	(   balanced_brackets(Goal, []) ->
	    writeln(' succeed')
	;
	    writeln(' failed')
	).

% grammar of balanced brackets
balanced_brackets --> [].

balanced_brackets -->
	['['],
	balanced_brackets,
	[']'].

balanced_brackets -->
	['[',']'],
	balanced_brackets.


% generator of random brackets
gen_bracket(0) --> [].

gen_bracket(N) -->
	{N1 is N - 1,
	 R is random(2)},
	bracket(R),
	gen_bracket(N1).

bracket(0) --> ['['].
bracket(1) --> [']'].
