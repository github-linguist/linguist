search_a_list(N1, N2) :-
	L = ["Zig", "Zag", "Wally", "Ronald", "Bush", "Krusty", "Charlie", "Bush", "Boz", "Zag"],

	write('List is :'), maplist(my_write, L), nl, nl,

	(   nth1(Ind1, L, N1) ->
	    format('~s is in position ~w~n', [N1, Ind1])
	;   format('~s is not present~n', [N1])),
	(   nth1(Ind2, L, N2) ->
	    format('~s is in position ~w~n', [N2, Ind2])
	;   format('~s is not present~n', [N2])),
	(   reverse_nth1(Ind3, L, N1) ->
	    format('~s last position is ~w~n', [N1, Ind3])
	;   format('~s is not present~n', [N1])).

reverse_nth1(Ind, L, N) :-
	reverse(L, RL),
	length(L, Len),
	nth1(Ind1, RL, N),
	Ind is Len - Ind1 + 1.

my_write(Name) :-
	writef(' %s', [Name]).
