huffman :-
	L = 'this is an example for huffman encoding',
	atom_chars(L, LA),
	msort(LA, LS),
	packList(LS, PL),
	sort(PL, PLS),
	build_tree(PLS, A),
	coding(A, [], C),
	sort(C, SC),
	format('Symbol~t   Weight~t~30|Code~n'),
	maplist(print_code, SC).

build_tree([[V1|R1], [V2|R2]|T], AF) :-
	V is V1 + V2,
	A = [V, [V1|R1], [V2|R2]],
	(   T=[] -> AF=A ;  sort([A|T], NT), build_tree(NT, AF) ).

coding([_A,FG,FD], Code, CF) :-
	(   is_node(FG) ->  coding(FG, [0 | Code], C1)
			 ;  leaf_coding(FG, [0 | Code], C1) ),
	(   is_node(FD) ->  coding(FD, [1 | Code], C2)
			 ;  leaf_coding(FD, [1 | Code], C2) ),
	append(C1, C2, CF).

leaf_coding([FG,FD], Code, CF) :-
	reverse(Code, CodeR),
	CF = [[FG, FD, CodeR]] .

is_node([_V, _FG, _FD]).

print_code([N, Car, Code]):-
	format('~w :~t~w~t~30|', [Car, N]),
	forall(member(V, Code), write(V)),
	nl.

packList([], []).
packList([X], [[1,X]]) :- !.
packList([X|Rest], [XRun|Packed]):-
    run(X, Rest, XRun, RRest),
    packList(RRest, Packed).

run(V, [], [1,V], []).
run(V, [V|LRest], [N1,V], RRest):-
    run(V, LRest, [N, V], RRest),
    N1 is N + 1.
run(V, [Other|RRest], [1,V], [Other|RRest]):-
    dif(V, Other).
