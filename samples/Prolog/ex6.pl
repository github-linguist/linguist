%6.8
subset(Set, Subset) :-
	append(L1, Subset, Set).
powerset(Set, Subset) :-
	bagof(Subset, subset(Set, Subset), Subset).
