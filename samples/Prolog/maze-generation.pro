:- dynamic cell/2.

maze(Lig,Col) :-
	retractall(cell(_,_)),

	new(D, window('Maze')),

	% creation of the grid
	forall(between(0,Lig, I),
	       (XL is  50, YL is I * 30 + 50,
		XR is Col * 30 + 50,
		new(L, line(XL, YL, XR, YL)),
		send(D, display, L))),

	forall(between(0,Col, I),
	       (XT is  50 + I * 30, YT is 50,
		YB is Lig * 30 + 50,
		new(L, line(XT, YT, XT, YB)),
		send(D, display, L))),

	SX is Col * 30 + 100,
	SY is Lig * 30 + 100,
	send(D, size, new(_, size(SX, SY))),

	% choosing a first cell
	L0 is random(Lig),
	C0 is random(Col),
	assert(cell(L0, C0)),
	\+search(D, Lig, Col, L0, C0),
	send(D, open).

search(D, Lig, Col, L, C) :-
	Dir is random(4),
	nextcell(Dir, Lig, Col, L, C, L1, C1),
	assert(cell(L1,C1)),
	assert(cur(L1,C1)),
	erase_line(D, L, C, L1, C1),
	search(D, Lig, Col, L1, C1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
erase_line(D, L, C, L, C1) :-
	(   C < C1 -> C2 = C1; C2 = C),
	XT is C2  * 30 + 50,
	YT is L * 30 + 51, YR is (L+1) * 30 + 50,
	new(Line, line(XT, YT, XT, YR)),
	send(Line, colour, white),
	send(D, display, Line).

erase_line(D, L, C, L1, C) :-
	XT is  51 + C * 30, XR is 50 + (C + 1) * 30,
	(   L < L1 -> L2 is L1; L2 is L),
	YT is L2 * 30 + 50,
	new(Line, line(XT, YT, XR, YT)),
	send(Line, colour, white),
	send(D, display, Line).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
nextcell(Dir, Lig, Col, L, C, L1, C1) :-
	next(Dir, Lig, Col, L, C, L1, C1);
	(   Dir1 is (Dir+3) mod 4,
	    next(Dir1, Lig, Col, L, C, L1, C1));
	(   Dir2 is (Dir+1) mod 4,
	    next(Dir2, Lig, Col, L, C, L1, C1));
	(   Dir3 is (Dir+2) mod 4,
	    next(Dir3, Lig, Col, L, C, L1, C1)).

% 0 => northward
next(0, _Lig, _Col, L, C, L1, C) :-
	L > 0,
	L1 is L - 1,
	\+cell(L1, C).

% 1 => rightward
next(1, _Lig, Col, L, C, L, C1) :-
	C < Col - 1,
	C1 is C + 1,
	\+cell(L, C1).

% 2 => southward
next(2, Lig, _Col, L, C, L1, C) :-
	L < Lig - 1,
	L1 is L + 1,
	\+cell(L1, C).

% 3 => leftward
next(2, _Lig, _Col, L, C, L, C1) :-
	C > 0,
	C1 is C - 1,
	\+cell(L, C1).
