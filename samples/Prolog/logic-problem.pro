/**
 * Question 1.1
 * combiner(+Buddies, -Pairs)
 */
combiner([], []).
combiner([First|Buddies], Pairs):-
	make_pairs(First, Buddies, Pairs1),
	combiner(Buddies, Pairs2),
	concat(Pairs1, Pairs2, Pairs).

/**
 * make_pairs(+Buddy, +Buddies, -Pairs)
 */
make_pairs(Buddy, [], []).
make_pairs(Buddy, [First|Buddies], [(Buddy, First)|Pairs]):-
	make_pairs(Buddy, Buddies, Pairs).

/**
 * concat(+X, +Y, ?T)
 */
concat([], Y, Y).
concat([P|R], Y, [P|T]):-
	concat(R, Y, T).


/**
 * Question 1.2
 * extraire(+AllPossiblePairs, +NbPairs, -Tp, -RemainingPairs)
 */
extraire(AllPossiblePairs, 0, [], AllPossiblePairs).
extraire([PossiblePair|AllPossiblePairs], NbPairs, [PossiblePair|Tp], NewRemainingPairs):-
	NbPairs > 0,
	NewNbPairs is NbPairs - 1,
	extraire(AllPossiblePairs, NewNbPairs, Tp, RemainingPairs),
	not(pair_in_array(PossiblePair, Tp)),
	delete_pair(RemainingPairs, PossiblePair, NewRemainingPairs).
extraire([PossiblePair|AllPossiblePairs], NbPairs, Tp, [PossiblePair|RemainingPairs]):-
	NbPairs > 0,
	extraire(AllPossiblePairs, NbPairs, Tp, RemainingPairs),
	pair_in_array(PossiblePair, Tp).

/**
 * delete_pair(+Pairs, +Pair, -PairsWithoutPair)
 */
delete_pair([], _, []).
delete_pair([Pair|Pairs], Pair, Pairs):-!.
delete_pair([FirstPair|Pairs], Pair, [FirstPair|PairsWithoutPair]):-
	delete_pair(Pairs, Pair, PairsWithoutPair).

/**
 * pair_in_array(+Pair, +Pairs)
 */
pair_in_array((A, B), [(C, D)|Pairs]):-
	(A == C ; B == D ; A == D ; B == C),
	!.
pair_in_array(Pair, [FirstPair|Pairs]):-
	pair_in_array(Pair, Pairs).


/**
 * Question 1.3
 * les_tps(+Buddies, -Tps)
 */
les_tps(Buddies, Tps):-
	combiner(Buddies, PossiblePairs),
	length(Buddies, NbBuddies),
	NbPairs is integer(NbBuddies / 2),
	findall(Tp, extraire(PossiblePairs, NbPairs, Tp, _), Tps).
