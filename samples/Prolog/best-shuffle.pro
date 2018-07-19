:- dynamic score/2.

best_shuffle :-
	maplist(best_shuffle, ["abracadabra", "eesaw", "elk", "grrrrrr",
			       	"up", "a"]).

best_shuffle(Str) :-
	retractall(score(_,_)),
	length(Str, Len),
	assert(score(Str, Len)),
	calcule_min(Str, Len, Min),
	repeat,
	   shuffle(Str, Shuffled),
	   maplist(comp, Str, Shuffled, Result),
	   sumlist(Result, V),
	   retract(score(Cur, VCur)),
	   (  V < VCur -> assert(score(Shuffled, V)); assert(score(Cur, VCur))),
	   V = Min,
	retract(score(Cur, VCur)),
	writef('%s : %s (%d)\n', [Str, Cur, VCur]).

comp(C, C1, S):-
	(   C = C1 -> S = 1; S = 0).

% this code was written by P.Caboche and can be found here :
% http://pcaboche.developpez.com/article/prolog/listes/?page=page_3#Lshuffle
shuffle(List, Shuffled) :-
  length(List, Len),
  shuffle(Len, List, Shuffled).

shuffle(0, [], []) :- !.

shuffle(Len, List, [Elem|Tail]) :-
  RandInd is random(Len),
  nth0(RandInd, List, Elem),
  select(Elem, List, Rest),
  NewLen is Len - 1,
  shuffle(NewLen, Rest, Tail).


% letters are sorted out then packed
% If a letter is more numerous than the rest
% the min is the difference between the quantity of this letter and
% the sum of the quantity of the other letters
calcule_min(Str, Len, Min) :-
	msort(Str, SS),
	packList(SS, Lst),
	sort(Lst, Lst1),
	last(Lst1, [N, _]),
	(   N * 2 > Len -> Min is 2 * N - Len; Min = 0).



% almost the same code as in "run_length" page
packList([],[]).

packList([X],[[1,X]]) :- !.


packList([X|Rest],[XRun|Packed]):-
    run(X,Rest, XRun,RRest),
    packList(RRest,Packed).


run(Var,[],[1,Var],[]).

run(Var,[Var|LRest],[N1, Var],RRest):-
    run(Var,LRest,[N, Var],RRest),
    N > 0,
    N1 is N + 1.


run(Var,[Other|RRest], [1,Var],[Other|RRest]):-
     dif(Var,Other).
