:- use_module(library( http/http_open )).

anagrams:-
        % we read the URL of the words
	http_open('http://www.puzzlers.org/pub/wordlists/unixdict.txt',	In, []),
	read_file(In, [], Out),
	close(In),

        % we get a list of pairs key-value where key = a-word value = <list-of-its-codes>
        % this list must be sorted
	msort(Out, MOut),

        % in order to gather values with the same keys
	group_pairs_by_key(MOut, GPL),

        % we sorted this list in decreasing order of the length of values
	predsort(my_compare, GPL, GPLSort),

	% we extract the first 6 items
        GPLSort = [_H1-T1, _H2-T2, _H3-T3, _H4-T4, _H5-T5, _H6-T6 | _],

        % Tnn are lists of codes (97 for 'a'), we create the strings
	maplist(maplist(atom_codes), L, [T1, T2, T3, T4, T5, T6] ),

	maplist(writeln, L).


read_file(In, L, L1) :-
	read_line_to_codes(In, W),
	(   W == end_of_file ->
               % the file is read
	       L1 = L
	       ;
               % we sort the list of codes of the line
	       msort(W, W1),

               % to create the key in alphabetic order
	       atom_codes(A, W1),

               % and we have the pair Key-Value in the result list
	       read_file(In, [A-W | L], L1)).

% predicate for sorting list of pairs Key-Values
% if the lentgh of values is the same
% we sort the keys in alhabetic order
my_compare(R, K1-V1, K2-V2) :-
	length(V1, L1),
	length(V2, L2),
	(   L1 < L2 -> R = >; L1 > L2 -> R = <; compare(R, K1, K2)).
