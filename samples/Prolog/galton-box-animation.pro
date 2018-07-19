:- dynamic tubes/1.
:- dynamic balls/2.
:- dynamic stop/1.

% number of rows of pins (0 -> 9)
row(9).

galton_box :-
	retractall(tubes(_)),
	retractall(balls(_,_)),
	retractall(stop(_)),
	assert(stop(@off)),
	new(D, window('Galton Box')),
	send(D, size, size(520,700)),
	display_pins(D),
	new(ChTubes, chain),
	assert(tubes(ChTubes)),
	display_tubes(D, ChTubes),
	new(Balls, chain),
	new(B, ball(D)),
	send(Balls, append, B),
	assert(balls(Balls, D)),
	send(D, done_message, and(message(ChTubes, clear),
				  message(ChTubes, free),
				  message(Balls, for_all, message(@arg1, free)),
				  message(Balls, clear),
				  message(Balls, free),
				  message(@receiver, destroy))),
	send(D, open).

% class pin, balls travel between pins
:- pce_begin_class(pin, circle, "pin").

initialise(P, Pos) :->
	send(P, send_super, initialise, 18),
	send(P, fill_pattern, new(_, colour(@default, 0, 0, 0))),
	get(Pos, x, X),
	get(Pos, y, Y),
	send(P, geometry, x := X, y := Y).
:- pce_end_class.


% class tube, balls fall in them
:- pce_begin_class(tube, path, "tube where balls fall").

variable(indice, any, both, "index of the tube in the list").
variable(balls, any, both, "number of balls inside").

initialise(P, Ind, D) :->
	row(Row),
	send(P, send_super, initialise, kind := poly),
	send(P, slot, balls, 0),
	send(P, slot, indice, Ind),
	X0 is 228 - Row * 20 + Ind * 40,
	X1 is X0 + 20,
	Y1 is 600, Y0 is 350,
	send_list(P, append, [point(X0, Y0), point(X0, Y1),
			      point(X1,Y1), point(X1,Y0)]),
	send(D, display, P).

% animation stop when a tube is full
add_ball(P) :->
	get(P, slot, balls, B),
	B1 is B+1,
	send(P, slot, balls, B1),
	(   B1 = 12
	->  retract(stop(_)), assert(stop(@on))
	;   true).
:- pce_end_class.


% class ball
:- pce_begin_class(ball, circle, "ball").

variable(angle, any, both, "angle of the ball with the pin").
variable(dir, any, both, "left / right").
variable(pin, point, both, "pin under the ball when it falls").
variable(position, point, both, "position of the ball").
variable(max_descent, any, both, "max descent").
variable(state, any, both, "in_pins / in_tube").
variable(window, any, both, "window to display").
variable(mytimer, timer, both, "timer of the animation").

initialise(P, W) :->
	send(P, send_super, initialise, 18),
	send(P, pen, 0),
	send(P, state, in_pins),
	send(P, fill_pattern, new(_, colour(@default, 65535, 0, 0))),
	Ang is 3 * pi / 2,
	send(P, slot, angle, Ang),
	send(P, slot, window, W),
	send(P, geometry, x := 250, y := 30),
	send(P, pin, point(250, 50)),
	send(P, choose_dir),
	send(P, mytimer, new(_, timer(0.005, message(P, move_ball)))),
	send(W, display, P),
	send(P?mytimer, start).

% method called when the object is destroyed
% first the timer is stopped
% then all the resources are freed
unlink(P) :->
	send(P?mytimer, stop),
	send(P, send_super, unlink).

choose_dir(P) :->
	I is random(2),
	(   I = 1 -> Dir = left; Dir = right),
	send(P, dir, Dir).

move_ball(P) :->
	get(P, state, State),
	(   State = in_pins
	->  send(P, move_ball_in_pins)
	;   send(P, move_ball_in_tube)).

move_ball_in_pins(P) :->
	get(P, slot, angle, Ang),
	get(P, slot, pin, Pin),
	get(P, slot, dir, Dir),
	(   Dir = left -> Ang1 is Ang-0.15 ; Ang1 is Ang + 0.15),
	get(Pin, x, PX),
	get(Pin, y, PY),
	X is 21 * cos(Ang1) +  PX,
	Y is 21 * sin(Ang1) + PY,
	send(P, geometry, x := X, y := Y),
	send(P?window, display, P),
	(   abs(Ang1 - pi) < 0.1
	->  PX1 is PX - 20,
	    send(P, next_move, PX1, PY)
	;   abs(Ang1 - 2 * pi) < 0.1
	->  PX1 is PX + 20,
	    send(P, next_move, PX1, PY)
	;   send(P, slot, angle, Ang1)).

next_move(P, PX, PY) :->
	row(Row),

	    Ang2 is 3 * pi / 2,
	    PY1 is PY + 30,
	    (	PY1 =:= (Row + 1) * 30 + 50
	    ->	send(P, slot, state, in_tube),
		NumTube is round((PX - 228 + Row * 20) / 40),
		tubes(ChTubes),
		get(ChTubes, find,
		    message(@prolog, same_value,@arg1?indice, NumTube),
		    Tube),
		send(Tube, add_ball),
		get(Tube, slot, balls, Balls),
		Max_descent is 600 - Balls * 20,
		send(P, slot, max_descent, Max_descent),
		send(P, slot, position, point(PX, PY))
	    ;	send(P, choose_dir),
		send(P, slot, angle, Ang2),
		send(P, slot, pin, point(PX, PY1))).

move_ball_in_tube(P) :->
	get(P, slot, position, Descente),
	get(Descente, x, PX1),
	get(Descente, y, PY),
	PY1 is PY+4,
	send(P, geometry, x := PX1, y := PY1),
	get(P, slot, max_descent, Max_descent),
	(   Max_descent =< PY1
	->  send(P?mytimer, stop),
	    (	stop(@off) ->  send(@prolog, next_ball); true)
	;   send(P, slot, position, point(PX1, PY1))),
	send(P?window, display, P).

:- pce_end_class.


next_ball :-
	retract(balls(Balls, D)),
	new(B, ball(D)),
	send(Balls, append, B),
	assert(balls(Balls, D)).

% test to find the appropriate tube
same_value(V, V).

display_pins(D) :-
	row(Row),
	forall(between(0, Row, I),
	       (  Start is 250 - I * 20,
		  Y is I * 30 + 50,
	          forall(between(0, I, J),
			 (   X is Start + J * 40,
			     new(P, pin(point(X,Y))),
			     send(D, display, P))))).

display_tubes(D, Ch) :-
	row(Row),
	Row1 is Row+1,
	forall(between(0, Row1, I),
	       (   new(T, tube(I, D)),
		   send(Ch, append, T),
		   send(D, display, T))).
