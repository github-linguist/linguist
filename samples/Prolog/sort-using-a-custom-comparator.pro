rosetta_sort :-
	L = ["Here", "are", "some", "sample", "strings", "to", "be", "sorted" ],
	predsort(my_comp, L, L1),
	writeln('Input list :'),
	maplist(my_write, L), nl,nl,
	writeln('Sorted list :'),
	maplist(my_write, L1).


my_comp(Comp, W1, W2) :-
	length(W1,L1),
	length(W2, L2),
	(   L1 < L2 -> Comp = '>'
	;   L1 > L2 -> Comp = '<'
	;   compare(Comp, W1, W2)).

my_write(W) :-
	format('~s ', [W]).
