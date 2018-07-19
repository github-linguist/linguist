ying_yang(N) :-
	R is N * 100,
	sformat(Title, 'Yin Yang ~w', [N]),
	new(W, window(Title)),
	new(Wh, colour(@default, 255*255, 255*255, 255*255)),
	new(Bl, colour(@default, 0, 0, 0)),
	CX is R + 50,
	CY is R + 50,
	R1 is R / 2,
	R2 is R / 8,
	CY1 is R1 + 50,
	CY2 is 3 * R1 + 50,

	new(E, semi_disk(point(CX, CY), R, w, Bl)),
	new(F, semi_disk(point(CX, CY), R, e, Wh)),
	new(D1, disk(point(CX, CY1), R, Bl)),
	new(D2, disk(point(CX, CY2), R, Wh)),
	new(D3, disk(point(CX, CY1), R2, Wh)),
	new(D4, disk(point(CX, CY2), R2, Bl)),

	send_list(W, display, [E, F, D1, D2, D3, D4]),

	WD is 2 * R + 100,
	send(W, size, size(WD, WD )),
	send(W, open).

:- pce_begin_class(semi_disk, path, "Semi disk with color ").

initialise(P, C, R, O, Col) :->
        send(P, send_super, initialise),
	get(C, x, CX),
	get(C, y, CY),
	choose(O, Deb, End),
	forall(between(Deb, End, I),
	       (   X is R * cos(I * pi/180) + CX,
		   Y is R * sin(I * pi/180) + CY,
	           send(P, append, point(X,Y)))),
	send(P, closed, @on),
	send(P, fill_pattern, Col).

:- pce_end_class.

choose(s, 0, 180).
choose(n, 180, 360).
choose(w, 90, 270).
choose(e, -90, 90).

:- pce_begin_class(disk, ellipse, "disk with color ").

initialise(P, C, R, Col) :->
        send(P, send_super, initialise, R, R),
	send(P, center, C),
	send(P, pen, 0),
	send(P, fill_pattern, Col).

:- pce_end_class.
