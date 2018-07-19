% the test
run_length :-
	L = "WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW",
	writef('encode %s\n', [L]),
	encode(L, R),
	writeln(R), nl,
	writef('decode %w\n', [R]),
	decode(R, L1),
	writeln(L1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  encode
%
%  translation
%  from
%  "WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW"
%  to
%  "12W1B12W3B24W1B14W"
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
encode(In, Out) :-
	% Because of the special management of the "strings" by Prolog
	( is_list(In) -> I = In; string_to_list(In, I)),
	packList(I, R1),
	dcg_packList2List(R1,R2, []),
	string_to_list(Out,R2).



dcg_packList2List([[N, V]|T]) -->
	{ number_codes(N, LN)},
	LN,
	[V],
	dcg_packList2List(T).

dcg_packList2List([]) --> [].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  decode
%
%  translation
%  from
%  "12W1B12W3B24W1B14W"
%  to
%  "WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWBWWWWWWWWWWWWWW"
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
decode(In, Out) :-
	% Because of the special management of the "strings" by Prolog
	( is_list(In) -> I = In; string_to_list(In, I)),
	dcg_List2packList(I, R1, []),
	packList(L1, R1),
	string_to_list(Out, L1).


dcg_List2packList([H|T]) -->
	{code_type(H, digit)},
	parse_number([H|T], 0).

dcg_List2packList([]) --> [].


parse_number([H|T], N) -->
	{code_type(H, digit), !,
	N1 is N*10 + H - 48 },
	parse_number(T, N1).

parse_number([H|T], N) -->
	[[N, H]],
	dcg_List2packList(T).


% use of library clpfd allows packList(?In, ?Out) to works
% in both ways In --> Out and In <-- Out.

:- use_module(library(clpfd)).

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


run(Var,[],[1,Var],[]).

run(Var,[Var|LRest],[N1, Var],RRest):-
    N #> 0,
    N1 #= N + 1,
    run(Var,LRest,[N, Var],RRest).


run(Var,[Other|RRest], [1,Var],[Other|RRest]):-
    dif(Var,Other).
