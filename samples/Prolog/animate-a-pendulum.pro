:- use_module(library(pce)).

pendulum :-
	new(D, window('Pendulum')),
	send(D, size, size(560, 300)),
	new(Line, line(80, 50, 480, 50)),
	send(D, display, Line),
	new(Circle, circle(20)),
	send(Circle, fill_pattern,  colour(@default, 0, 0, 0)),
	new(Boule, circle(60)),
	send(Boule, fill_pattern,  colour(@default, 0, 0, 0)),
	send(D, display, Circle, point(270,40)),
	send(Circle, handle, handle(h/2, w/2, in)),
	send(Boule, handle, handle(h/2, w/2, out)),
	send(Circle, connect, Boule, link(in, out, line(0,0,0,0,none))),
	new(Anim, animation(D, 0.0, Boule, 200.0)),
	send(D, done_message, and(message(Anim, free),
				  message(Boule, free),
				  message(Circle, free),
				  message(@receiver,destroy))),
	send(Anim?mytimer, start),
	send(D, open).




:- pce_begin_class(animation(window, angle, boule, len_pendulum), object).
variable(window, object,  both, "Display window").
variable(boule,  object, both,  "bowl of the pendulum").
variable(len_pendulum,    object, both,  "len of the pendulum").
variable(angle,  object, both,  "angle with the horizontal").
variable(delta,    object, both,  "increment of the angle").
variable(mytimer, timer, both, "timer of the animation").

initialise(P, W:object, A:object, B : object, L:object) :->
        "Creation of the object"::
        send(P, window, W),
        send(P, angle, A),
        send(P, boule, B),
        send(P, len_pendulum, L),
        send(P, delta, 0.01),
	send(P, mytimer, new(_, timer(0.01,message(P, anim_message)))).

% method called when the object is destroyed
% first the timer is stopped
% then all the resources are freed
unlink(P) :->
	send(P?mytimer, stop),
	send(P, send_super, unlink).


% message processed by the timer
anim_message(P) :->
	get(P, angle, A),
	get(P, len_pendulum, L),
	calc(A, L, X, Y),
	get(P, window, W),
	get(P, boule, B),
	send(W, display, B, point(X,Y)),
	% computation of the next position
	get(P, delta, D),
	next_Angle(A, D, NA, ND),
	send(P, angle, NA),
	send(P, delta, ND).

:- pce_end_class.

% computation of the position of the bowl.
calc(Ang, Len, X, Y) :-
	X is Len * cos(Ang)+ 250,
	Y is Len * sin(Ang) + 20.


% computation of the next angle
% if we reach 0 or pi, delta change.
next_Angle(A, D, NA, ND) :-
	NA is D + A,
	(((D > 0,   abs(pi-NA) < 0.01); (D < 0, abs(NA) < 0.01))->
	  ND = - D;
	  ND = D).
