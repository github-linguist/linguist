strip :-
	In = "    There are unwanted blanks here!    ",
	strip_left(In, OutLeft),
	format('In          : ~s__~n', [In]),
	format('Strip left  : ~s__~n', [OutLeft]),
	strip_right(In, OutRight),
	format('Strip right : ~s__~n', [OutRight]),
	strip(In, Out),
	format('Strip       : ~s__~n', [Out]).


strip_left(In, Out) :-
	strip_action(In, Out, []).

strip_right(In, Out) :-
	reverse(In, RIn),
	strip_left(RIn, ROut),
	reverse(ROut, Out).

strip(In, Out) :-
	strip_left(In, Tmp),
	strip_right(Tmp, Out).

strip_action([X|T]) -->
	{\+code_type(X, graph), !},
	strip_action(T).


strip_action(X) --> X.
