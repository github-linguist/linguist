ctail(_, [], Rev, Rev, sorted) :- write(Rev), nl.
ctail(fwrd, [A,B|T], In, Rev, unsorted) :- A > B, !,
	ctail(fwrd, [B,A|T], In, Rev, _).
ctail(bkwd, [A,B|T], In, Rev, unsorted) :- A < B, !,
	ctail(bkwd, [B,A|T], In, Rev, _).
ctail(D,[A|T], In, Rev, Ch) :- !, ctail(D, T, [A|In], Rev, Ch).

cocktail([], []).
cocktail(In, [Min|Out]) :-
	ctail(fwrd, In, [], [Max|Rev], SFlag),
	( SFlag=sorted->reverse([Max|Rev], [Min|Out]);
	 (ctail(bkwd, Rev, [Max], [Min|Tmp], SortFlag),
	  (SortFlag=sorted->Out=Tmp; !, cocktail(Tmp, Out)))).

test :-  In = [8,9,1,3,4,2,6,5,4],
	 writef('  input=%w\n', [In]),
	 cocktail(In, R),
	 writef('-> %w\n', [R]).
