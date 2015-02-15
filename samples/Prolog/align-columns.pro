aligner :-
	L ="Given$a$text$file$of$many$lines,$where$fields$within$a$line$
are$delineated$by$a$single$'dollar'$character,$write$a$program
that$aligns$each$column$of$fields$by$ensuring$that$words$in$each$
column$are$separated$by$at$least$one$space.
Further,$allow$for$each$word$in$a$column$to$be$either$left$
justified,$right$justified,$or$center$justified$within$its$column.",

	% read the lines and the words
	% compute the length of the longuest word.
	% LP is the list of lines,
	% each line is a list of words
	parse(L, 0, N, LP, []),

	% we need to add 1 to aligned
	N1 is N+1,
	% words will be left aligned
	sformat(AL, '~~w~~t~~~w|', [N1]),
	% words will be centered
	sformat(AC, '~~t~~w~~t~~~w|', [N1]),
	% words will be right aligned
	sformat(AR, '~~t~~w~~~w|', [N1]),

	write('Left justified :'), nl,
	maplist(affiche(AL), LP), nl,
	write('Centered justified :'), nl,
	maplist(affiche(AC), LP), nl,
	write('Right justified :'), nl,
	maplist(affiche(AR), LP), nl.

affiche(F, L) :-
	maplist(my_format(F), L),
	nl.

my_format(_F, [13]) :-
	nl.

my_format(F, W) :-
	string_to_atom(W,AW),
	sformat(AF, F, [AW]),
	write(AF).


parse([], Max, Max) --> [].

parse(T, N, Max) -->
	{ parse_line(T, 0, N1, T1, L, []),
	  (   N1 > N -> N2 = N1; N2 = N)},
	[L],
	parse(T1, N2, Max).

parse_line([], NF, NF, []) --> [].

parse_line([H|TF], NF, NF, TF) -->
	{code_type(H, end_of_line), !},
	[].


parse_line(T, N, NF, TF) -->
	{ parse_word(T, 0, N1, T1, W, []),
	  (   N1 > N -> N2 = N1; N2 = N)},
	[W],
	parse_line(T1, N2, NF, TF).

% 36 is the code of '$'
parse_word([36|T], N, N, T) -->
	{!},
	[].

parse_word([H|T], N, N, [H|T]) -->
	{code_type(H, end_of_line), !},
	[].

parse_word([], N, N, []) --> [].

parse_word([H|T], N1, NF, TF) -->
	[H],
	{N2 is  N1 + 1},
	parse_word(T, N2, NF, TF).
