:- use_module(library(clpfd)).

self_describling :-
	forall(between(1, 10, I),
	       (findall(N, self_describling(I,N), L),
		format('Len ~w, Numbers ~w~n', [I, L]))).

% search of the self_describling numbers of a given len
self_describling(Len, N) :-
	length(L, Len),
	Len1 is Len - 1,
	L = [H|T],

	% the first figure is greater than 0
	H in 1..Len1,

	% there is a least to figures so the number of these figures
	% is at most Len - 2
	Len2 is Len - 2,
	T ins 0..Len2,

	% the sum of the figures is equal to the len of the number
	sum(L, #=, Len),

	% There is at least one figure corresponding to the number of zeros
	H1 #= H+1,
	element(H1, L, V),
	V #> 0,

	% create the list
	label(L),

	% test the list
	msort(L, LNS),
	packList(LNS,LNP),
	numlist(0, Len1, NumList),
	verif(LNP,NumList, L),

	% list is OK, create the number
	maplist(atom_number, LA, L),
	number_chars(N, LA).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% testing a number (not use in this program)
self_describling(N) :-
	number_chars(N, L),
	maplist(atom_number, L, LN),
	msort(LN, LNS),
	packList(LNS,LNP), !,
	length(L, Len),
	Len1 is Len - 1,
	numlist(0, Len1, NumList),
	verif(LNP,NumList, LN).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% verif(PackList, Order_of_Numeral, Numeral_of_the_nuber_to_test)
%  Packlist is of the form [[Number_of_Numeral, Order_of_Numeral]|_]
%  Test succeed when

%  All lists are empty
verif([], [], []).

% Packlist is empty and all lasting numerals are 0
verif([], [_N|S], [0|T]) :-
	verif([], S, T).

% Number of numerals N is V
verif([[V, N]|R], [N|S], [V|T]) :-
	verif(R, S, T).

% Number of numerals N is 0
verif([[V, N1]|R], [N|S], [0|T]) :-
	N #< N1,
	verif([[V,N1]|R], S, T).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% ?- packList([a,a,a,b,c,c,c,d,d,e], L).
%  L = [[3,a],[1,b],[3,c],[2,d],[1,e]] .
% ?- packList(R,  [[3,a],[1,b],[3,c],[2,d],[1,e]]).
% R = [a,a,a,b,c,c,c,d,d,e] .
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
packList([],[]).

packList([X],[[1,X]]) :- !.


packList([X|Rest],[XRun|Packed]):-
    run(X,Rest, XRun,RRest),
    packList(RRest,Packed).


run(Var,[],[1, Var],[]).

run(Var,[Var|LRest],[N1, Var],RRest):-
    N #> 0,
    N1 #= N + 1,
    run(Var,LRest,[N, Var],RRest).


run(Var,[Other|RRest], [1, Var],[Other|RRest]):-
    dif(Var,Other).
