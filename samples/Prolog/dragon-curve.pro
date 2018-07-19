dragonCurve(N) :-
	dcg_dg(N, [left], DCL, []),
	Side = 4,
	Angle is -N * (pi/4),
	dcg_computePath(Side, Angle, DCL, point(180,400), P, []),
	new(D, window('Dragon Curve')),
	send(D, size, size(800,600)),
	new(Path, path(poly)),
	send_list(Path, append, P),
	send(D, display, Path),
	send(D, open).


% compute the list of points of the Dragon Curve
dcg_computePath(Side, Angle, [left | DCT], point(X1, Y1)) -->
	   [point(X1, Y1)],
	   {	X2 is X1 + Side * cos(Angle),
		Y2 is Y1 + Side * sin(Angle),
		Angle1 is Angle + pi / 2
	   },
	   dcg_computePath(Side, Angle1, DCT, point(X2, Y2)).

dcg_computePath(Side, Angle, [right | DCT], point(X1, Y1)) -->
	   [point(X1, Y1)],
	   {	X2 is X1 + Side * cos(Angle),
		Y2 is Y1 + Side * sin(Angle),
		Angle1 is Angle - pi / 2
	   },
	   dcg_computePath(Side, Angle1, DCT, point(X2, Y2)).


dcg_computePath(_Side, _Angle, [], point(X1, Y1)) -->
	[ point(X1, Y1)].


% compute the list of the "turns" of the Dragon Curve
dcg_dg(1, L) --> L.

dcg_dg(N, L) -->
	{dcg_dg(L, L1, []),
	  N1 is N - 1},
	  dcg_dg(N1, L1).

% one interation of the process
dcg_dg(L) -->
	L,
	[left],
	inverse(L).

inverse([H | T]) -->
	inverse(T),
	inverse(H).

inverse([]) --> [].

inverse(left) -->
	[right].

inverse(right) -->
	[left].
