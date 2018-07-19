fractal :-
	new(D, window('Fractal')),
	send(D, size, size(800, 600)),
	drawTree(D, 400, 500, -90, 9),
	send(D, open).


drawTree(_D, _X, _Y, _Angle, 0).

drawTree(D, X1, Y1, Angle, Depth) :-
        X2 is X1 + cos(Angle * pi / 180.0) * Depth * 10.0,
        Y2 is Y1 + sin(Angle * pi / 180.0) * Depth * 10.0,
	new(Line, line(X1, Y1, X2, Y2, none)),
	send(D, display, Line),
	A1 is Angle - 30,
	A2 is Angle + 30,
	De is Depth - 1,
        drawTree(D, X2, Y2, A1, De),
        drawTree(D, X2, Y2, A2, De).
