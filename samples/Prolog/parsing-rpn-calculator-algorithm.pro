rpn(L) :-
	writeln('Token  Action                             Stack'),
	parse(L, [],[X] ,[]),
	format('~nThe final output value is ~w~n', [X]).

% skip spaces
parse([X|L], St) -->
	{char_type(X, white)},
	parse(L, St).

% detect operators
parse([Op|L], [Y, X | St]) -->
	{ is_op(Op, X, Y, V),
	  writef('    %s', [[Op]]),
	  with_output_to(atom(Str2), writef('Apply %s on top of stack', [[Op]])),
	  writef('  %35l', [Str2]),
	  writef('%w\n', [[V | St]])},
	parse(L, [V | St]).

% detect number
parse([N|L], St) -->
	{char_type(N, digit)},
	parse_number(L, [N], St).

% string is finished
parse([], St) --> St.

% compute numbers
parse_number([N|L], NC, St) -->
	{char_type(N, digit)},
	parse_number(L, [N|NC], St).

parse_number(S, NC, St) -->
	{ reverse(NC, RNC),
	  number_chars(V, RNC),
	  writef('%5r', [V]),
	  with_output_to(atom(Str2), writef('Push num %w on top of stack', [V])),
	  writef('  %35l', [Str2]),
	  writef('%w\n', [[V | St]])},
	parse(S, [V|St]).

% defining operations
is_op(42, X, Y, V) :-	V is X*Y.
is_op(43, X, Y, V) :-	V is X+Y.
is_op(45, X, Y, V) :-	V is X-Y.
is_op(47, X, Y, V) :-	V is X/Y.
is_op(94, X, Y, V) :-	V is X**Y.
