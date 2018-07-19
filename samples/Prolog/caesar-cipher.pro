:- use_module(library(clpfd)).

caesar :-
	L1 = "The five boxing wizards jump quickly",
	writef("Original : %s\n", [L1]),

	% encryption of the sentence
	encoding(3, L1, L2) ,
	writef("Encoding : %s\n", [L2]),

	% deciphering on the encoded sentence
	encoding(3, L3, L2),
	writef("Decoding : %s\n", [L3]).

% encoding/decoding of a sentence
encoding(Key, L1, L2) :-
	maplist(caesar_cipher(Key), L1, L2).

caesar_cipher(_, 32, 32) :- !.

caesar_cipher(Key, V1, V2) :-
	V #= Key + V1,

	% we verify that we are in the limits of A-Z and a-z.
	((V1 #=< 0'Z #/\ V #> 0'Z) #\/ (V1 #=< 0'z #/\ V #> 0'z)
	#\/
	(V1 #< 0'A #/\ V2 #>= 0'A)#\/ (V1 #< 0'a #/\ V2 #>= 0'a)) #==> A,

	% if we are not in these limits A is 1, otherwise 0.
	V2 #= V - A * 26,

	% compute values of V1 and V2
	label([A, V1, V2]).
