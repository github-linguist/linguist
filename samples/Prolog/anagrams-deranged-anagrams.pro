longest_deranged_anagram :-
	http_open('http://www.puzzlers.org/pub/wordlists/unixdict.txt',In,[]),
	read_file(In, [], Out),
	close(In),
	msort(Out, MOut),
	group_pairs_by_key(MOut, GPL),
	map_list_to_pairs(compute_len, GPL, NGPL),
	predsort(my_compare, NGPL, GPLSort),
	search_derangement(GPLSort).


% order tuples to have longest words first
my_compare(R, N1-(K1-E1), N2-(K2-E2)) :-
	(   N1 < N2 -> R = > ; N1 > N2 -> R = <;
	length(E1, L1),
	length(E2, L2),
	(   L1 < L2 -> R = <; L1 > L2 -> R = >; compare(R, K1, K2))).


compute_len(_-[H|_], Len) :-
	length(H, Len).


% check derangement of anagrams
derangement([], []).
derangement([H1|T1], [H2 | T2]) :-
	H1 \= H2,
	derangement(T1, T2).


search_derangement([_-(_-L) | T]) :-
	length(L, 1), !,
	search_derangement(T).


search_derangement([_-(_-L) | T]) :-
	(   search(L) -> true; search_derangement(T)).

search([]) :- fail.
search([H | T]) :-
	(   search_one(H, T) -> true; search(T)).


search_one(Word, L) :-
	include(derangement(Word), L, [H|_]),
	atom_codes(W, Word),
	atom_codes(W1, H),
	format('Longest deranged anagrams : ~w ~w ~n', [W, W1]).


read_file(In, L, L1) :-
	read_line_to_codes(In, W),
	(   W == end_of_file ->
	       L1 = L
	       ;
	       msort(W, W1),
	       atom_codes(A, W1),
	       read_file(In, [A-W | L], L1)).
