frequency(File) :-
	read_file_to_codes(File, Code, []),

	% we only keep alphabetic codes
	include(my_code_type, Code, LstCharCode),

	% we translate char_codes into uppercase atoms.
	maplist(my_upcase, LstCharCode, LstChar),

	% sort and pack the list
	msort(LstChar, SortLstChar),
	packList(SortLstChar, Freq),
	maplist(my_write, Freq).


my_write([Num, Atom]) :-
	swritef(A, '%3r', [Num]),
	writef('Number of %w :%w\n', [Atom, A]).


my_code_type(Code) :-
	code_type(Code, alpha).

my_upcase(CharCode, UpChar) :-
	char_code(Atom, CharCode),
	upcase_atom(Atom, UpChar).

:- use_module(library(clpfd)).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ?- packList([a,a,a,b,c,c,c,d,d,e], L).
%  L = [[3,a],[1,b],[3,c],[2,d],[1,e]] .
%
% ?- packList(R,  [[3,a],[1,b],[3,c],[2,d],[1,e]]).
% R = [a,a,a,b,c,c,c,d,d,e] .
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
packList([],[]).

packList([X],[[1,X]]) :-
	!.

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
