:- use_module(library(lambda)).

:- dynamic res/3.

last_first(Len, Nb, L) :-
	retractall(res(_,_,_)),
	assert(res(0, 0, [])),
	% compute all the lists of connected words
	last_first,
	res(Len, Nb, L1),
	% to have only the words
	maplist(\X^Y^(X = [Y, _, _]), L1, L).

% create  the lists of connected words (initiate the first word)
last_first :-
	init(L),
	forall(select(Word, L, L1),
	       \+lance_p([Word | L1])).

% compute all the lists beginning with a word
% memorize the longest
lance_p(L) :-
	p(LF, L),
	retract(res(Len, Nb, Lst)),
	length(LF, Len1),
	(  Len1 > Len
	->  assert(res(Len1, 1, LF))
	;   Len1 = Len
	->  Nb1 is Nb + 1,
	    assert(res(Len, Nb1, Lst))
	;   assert(res(Len, Nb, Lst))),
	fail.

% describe the property of the list of connected words
p([A | T], [A | L]) :-
	select(B, L, L1),
	p0(A,B),
	T = [B | T1],
	p([B | T1], [B | L1]).

% a list with one element is valid
p([_], _).


% are words conected ?
p0([_, _, W], [_, W, _]).

% each word is associated with its first and last letters
% audino --> [audino, a, o]
init(L) :-

	L0 = [ audino, bagon, baltoy, banette, bidoof, braviary, bronzor,
	     carracosta, charmeleon, cresselia, croagunk, darmanitan, deino,
	     emboar, emolga, exeggcute, gabite, girafarig, gulpin, haxorus,
	     heatmor, heatran, ivysaur, jellicent, jumpluff, kangaskhan,
	     kricketune, landorus, ledyba, loudred, lumineon, lunatone,
	     machamp, magnezone, mamoswine, nosepass, petilil, pidgeotto,
	     pikachu, pinsir, poliwrath, poochyena, porygon2, porygonz,
	     registeel, relicanth, remoraid, rufflet, sableye, scolipede,
	     scrafty, seaking, sealeo, silcoon, simisear, snivy, snorlax,
	     spoink, starly, tirtouga, trapinch, treecko, tyrogue, vigoroth,
	     vulpix, wailord, wartortle, whismur, wingull, yamask],
	maplist(init_, L0, L).

% audino --> [audino, a, o]
init_(W, [W, F, L]) :-
	first_letter(W, F),
	last_letter(W, L).


first_letter(A, F) :-
	atom_chars(A, [F | _]).

last_letter(A, L) :-
	atom_chars(A, LC),
	reverse(LC, [L | _]).
