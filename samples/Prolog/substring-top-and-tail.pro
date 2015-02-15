remove_first_last_chars :-
	L = "Rosetta",
	L = [_|L1],
	remove_last(L, L2),
	remove_last(L1, L3),
	writef('Original string		 : %s\n', [L]),
	writef('Without first char       : %s\n', [L1]),
	writef('Without last char        : %s\n', [L2]),
	writef('Without first/last chars : %s\n', [L3]).

remove_last(L, LR) :-
	append(LR, [_], L).
