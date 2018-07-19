%_______________________________________________________________
% Langtons ant.
:-dynamic
	black/1.

plot_point(Row, Col) :-   % Output a 5x5 black box at R,C
	new(C, box(5,5)), X is Col * 5 - 2, Y is Row * 5 - 2,
	send(C, colour, colour(black)), send(C, fill_pattern, colour(blue)),
	send(C, center(point(X,Y))), send(@win, display, C).
update_win :-  % Make a 500x500 window, find all the black points and plot them
	new(@win, window('Langtons Ant')),
	send(@win, size, size(500,500)), send(@win, open),
	black(Row/Col),plot_point(Row,Col),fail.
update_win.

direction(Row, Col, left) :- black(Row/Col), !, retract(black(Row/Col)).
direction(Row, Col, right):- not(black(Row/Col)), !, assert(black(Row/Col)).

move(_, Row,Col) :- (Row < 0; Col < 0; Row > 99; Col > 99), !.
move(north,Row,Col) :-
	(direction(Row,Col,left), C is Col - 1, !, move(west, Row, C));
	(direction(Row,Col,right), C is Col + 1, !, move(east, Row, C)).
move(south,Row,Col) :-
	(direction(Row,Col,right), C is Col - 1, !, move(west, Row, C));
	(direction(Row,Col,left), C is Col + 1, !, move(east, Row, C)).
move(east,Row,Col) :-
	(direction(Row,Col,right), R is Row + 1, !, move(south, R, Col));
	(direction(Row,Col,left), R is Row - 1, !, move(north, R, Col)).
move(west,Row,Col) :-
	(direction(Row,Col,left), R is Row + 1, !, move(south, R, Col));
	(direction(Row,Col,right), R is Row - 1, !, move(north, R, Col)).

go :-   retractall(black(_)), move(north,49,49), update_win.
