cuboid(D1,D2,D3) :-
	W is D1 * 50,
	H is D2 * 50,
	D is D3 * 50,

	new(C, window(cuboid)),

	% compute the size of the window
	Width is W + ceiling(sqrt(H * 48)) + 50,
	Height is H +  ceiling(sqrt(H * 48)) + 50,
	send(C, size, new(_,size(Width,Height))),

	%compute the top-left corner of the front face of the cuboid
	PX is 25,
	PY is 25 + ceiling(sqrt(H * 48)),

	% colors of the faces
	new(C1, colour(@default, 65535, 0, 0)),
	new(C2, colour(@default, 0, 65535, 0)),
	new(C3, colour(@default, 0, 0, 65535)),

	% the front face
	new(B1, box(W, H)),
	send(B1, fill_pattern, C1),
	send(C, display,B1, point(PX, PY)),

	% the top face
	new(B2, hpara(point(PX,PY), W, D, C2)),
	send(C, display, B2),

	% the left face
	PX1 is PX + W,
	new(B3, vpara(point(PX1,PY), H, D, C3)),
	send(C, display, B3),

	send(C, open).



:- pce_begin_class(hpara, path, "drawing of a horizontal parallelogram").

initialise(P, Pos, Width, Hight, Color) :->
	send(P, send_super, initialise),
	send(P, append, Pos),
	H is ceiling(sqrt(Hight * 48)),
	get(Pos, x, X),
	get(Pos, y, Y),
	X1 is X + H,
	Y1 is Y - H,
	send(P, append, point(X1, Y1)),
	X2 is X1 + Width,
	send(P, append, point(X2, Y1)),
	X3 is X2 - H,
	send(P, append, point(X3, Pos?y)),
	send(P, append, Pos),
	send(P, fill_pattern, Color).

:- pce_end_class.

:- pce_begin_class(vpara, path, "drawing of a vertical parallelogram").

initialise(P, Pos, Hight, Depth, Color) :->
	send(P, send_super, initialise),
	send(P, append, Pos),
	H is ceiling(sqrt(Depth * 48)),
	get(Pos, x, X),
	get(Pos, y, Y),
	X1 is X + H,
	Y1 is Y - H,
	send(P, append, point(X1, Y1)),
	Y2 is Y1 + Hight,
	send(P, append, point(X1, Y2)),
	Y3 is Y2 + H,
	send(P, append, point(X, Y3)),
	send(P, append, Pos),
	send(P, fill_pattern, Color).

:- pce_end_class.
