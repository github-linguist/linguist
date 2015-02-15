:- use_module(library(clpfd)).

roman :-
	LA =  [    _       , 2010,    _, 1449,         _],
	LR =  ['MDCCLXXXIX',  _  , 'CX',    _, 'MDCLXVI'],
	maplist(roman,   LA, LR),
	maplist(my_print,LA, LR).


roman(A, R) :-
	A #> 0,
	roman(A, [u, t, h, th], LR, []),
	label([A]),
	parse_Roman(CR, LR, []),
	atom_chars(R, CR).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% using DCG

roman(0, []) --> [].

roman(N, [H | T]) -->
	{N1 #= N / 10,
	 N2 #= N mod 10},
	roman(N1, T),
	unity(N2, H).

unity(1, u) --> ['I'].
unity(1, t) --> ['X'].
unity(1, h) --> ['C'].
unity(1, th)--> ['M'].

unity(4, u) --> ['IV'].
unity(4, t) --> ['XL'].
unity(4, h) --> ['CD'].
unity(4, th)--> ['MMMM'].

unity(5, u) --> ['V'].
unity(5, t) --> ['L'].
unity(5, h) --> ['D'].
unity(5, th)--> ['MMMMM'].

unity(9, u) --> ['IX'].
unity(9, t) --> ['XC'].
unity(9, h) --> ['CM'].
unity(9, th)--> ['MMMMMMMMM'].

unity(0, _) --> [].


unity(V, U)-->
	{V #> 5,
	V1 #= V - 5},
	unity(5, U),
	unity(V1, U).

unity(V, U) -->
	{V #> 1, V #< 4,
	V1 #= V-1},
	unity(1, U),
	unity(V1, U).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Extraction of roman "lexeme"
parse_Roman(['C','M'|T]) -->
	['CM'],
	parse_Roman(T).

parse_Roman(['C','D'|T]) -->
	['CD'],
	parse_Roman(T).

parse_Roman(['X','C'| T]) -->
	['XC'],
	parse_Roman(T).


parse_Roman(['X','L'| T]) -->
	['XL'],
	parse_Roman(T).


parse_Roman(['I','X'| T]) -->
	['IX'],
	parse_Roman(T).


parse_Roman(['I','V'| T]) -->
	['IV'],
	parse_Roman(T).

parse_Roman([H | T]) -->
	[H],
	parse_Roman(T).


parse_Roman([]) -->
	[].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
my_print(A, R) :-
	format('~w in roman is ~w~n', [A, R]).
