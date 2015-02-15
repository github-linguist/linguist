:- use_module(library( http/http_open )).

ordered_words :-
        % we read the URL of the words
	http_open('http://www.puzzlers.org/pub/wordlists/unixdict.txt',	In, []),
	read_file(In, [], Out),
	close(In),

        % we get a list of pairs key-value where key = Length and value = <list-of-its-codes>
        % this list must be sorted
	msort(Out, MOut),

	group_pairs_by_key(MOut, POut),

       % we sorted this list in decreasing order of the length of values
	predsort(my_compare, POut, [_N-V | _OutSort]),
	maplist(mwritef, V).


mwritef(V) :-
	writef('%s\n', [V]).

read_file(In, L, L1) :-
	read_line_to_codes(In, W),
	(   W == end_of_file ->
               % the file is read
	       L1 = L
	       ;
               % we sort the list of codes of the line
	       % and keep only the "goods word"
	       (   msort(W, W) ->
	           length(W, N), L2 = [N-W | L], (len = 6 -> writef('%s\n', [W]); true)
	       ;
	           L2 = L
	       ),

               % and we have the pair Key-Value in the result list
	       read_file(In, L2, L1)).

% predicate for sorting list of pairs Key-Values
% if the lentgh of values is the same
% we sort the keys in alhabetic order
my_compare(R, K1-_V1, K2-_V2) :-
	(   K1 < K2 -> R = >; K1 > K2 -> R = <; =).
