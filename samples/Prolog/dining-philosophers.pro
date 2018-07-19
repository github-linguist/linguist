dining_philosophers :-
	new(D, window('Dining philosophers')),
	new(S, window('Dining philosophers : statistics')),
	send(D, size, new(_, size(800,800))),

	new(E, ellipse(400,400)),
	send(E, center, point(400,400)),
	send(D, display, E),

	new(F1, fork(0)),
	new(F2, fork(1)),
	new(F3, fork(2)),
	new(F4, fork(3)),
	new(F5, fork(4)),

	send_list(D, display, [F1,F2,F3,F4,F5]),

	new(Waiter, waiter(F1, F2, F3, F4, F5)),

	create_plate(P1, 0),
	create_plate(P2, 1),
	create_plate(P3, 2),
	create_plate(P4, 3),
	create_plate(P5, 4),

	create_point(0, Pt1),
	create_point(1, Pt2),
	create_point(2, Pt3),
	create_point(3, Pt4),
	create_point(4, Pt5),


	new(Ph1, philosopher('Aristotle', Waiter, P1, D, S, 0, Pt1, left)),
	new(Ph2, philosopher('Kant', Waiter, P2, D, S, 1, Pt2, left)),
	new(Ph3, philosopher('Spinoza', Waiter, P3, D, S, 2, Pt3, right)),
	new(Ph4, philosopher('Marx', Waiter, P4, D, S, 3, Pt4, right)),
	new(Ph5, philosopher('Russell', Waiter, P5, D, S, 4, Pt5, left)),

	send(Waiter, init_phi, Ph1, Ph2, Ph3, Ph4, Ph5),

	send_list([Ph1, Ph2, Ph3, Ph4, Ph5], start),

	send(D, done_message, and(message(Waiter, free),
				 message(Ph1, free),
				 message(Ph2, free),
				 message(Ph3, free),
				 message(Ph4, free),
				 message(Ph5, free),
				 message(S, open),
				 message(D, destroy))),

	send(D, open).


create_plate(P, N) :-
	new(P, ellipse(80,80)),
	X is 400 + 140 * cos(N * pi / 2.5),
	Y is 400 + 140 * sin(N * pi / 2.5),
	send(P, center, point(X, Y)).

create_point(N, point(X, Y)) :-
	X is 400 + 220 * cos(N * pi / 2.5),
	Y is 400 + 220 * sin(N * pi / 2.5) - 20.


:- pce_begin_class(waiter , object, "gives the forks to the philosophers").
variable(f1, fork, both, "free or used").
variable(f2, fork, both, "free or used").
variable(f3, fork, both, "free or used").
variable(f4, fork, both, "free or used").
variable(f5, fork, both, "free or used").
variable(phi1, philosopher, both, "philosopher").
variable(phi2, philosopher, both, "philosopher").
variable(phi3, philosopher, both, "philosopher").
variable(phi4, philosopher, both, "philosopher").
variable(phi5, philosopher, both, "philosopher").

initialise(P, F1, F2, F3, F4, F5) :->
	send(P, slot, f1, F1),
	send(P, slot, f2, F2),
	send(P, slot, f3, F3),
	send(P, slot, f4, F4),
	send(P, slot, f5, F5).

init_phi(P, Phi1,Phi2, Phi3, Phi4, Phi5) :->
	send(P, slot, phi1, Phi1),
	send(P, slot, phi2, Phi2),
	send(P, slot, phi3, Phi3),
	send(P, slot, phi4, Phi4),
	send(P, slot, phi5, Phi5).


want_forks(P, Phi) :->
	( get(P, slot, phi1, Phi) ,!, check_forks(P, Phi, f5, f1);
	 get(P, slot, phi2, Phi),!, check_forks(P, Phi, f1, f2);
	 get(P, slot, phi3, Phi),!, check_forks(P, Phi, f2, f3);
	 get(P, slot, phi4, Phi),!, check_forks(P, Phi, f3, f4);
	 get(P, slot, phi5, Phi),!, check_forks(P, Phi, f4, f5)).




give_back_forks(P, Phi) :->
	( get(P, slot, phi1, Phi) ,!, release_forks(P, phi1);
	 get(P, slot, phi2, Phi),!, release_forks(P, phi2);
	 get(P, slot, phi3, Phi),!, release_forks(P, phi3);
	 get(P, slot, phi4, Phi),!, release_forks(P, phi4);
	 get(P, slot, phi5, Phi),!, release_forks(P, phi5)),

	get(P, slot, phi1, Phi1),
	check_forks(P, Phi1, f5, f1),
	get(P, slot, phi2, Phi2),
	check_forks(P, Phi2, f1, f2),
	get(P, slot, phi3, Phi3),
	check_forks(P, Phi3, f2, f3),
	get(P, slot, phi4, Phi4),
	check_forks(P, Phi4, f3, f4),
	get(P, slot, phi5, Phi5),
	check_forks(P, Phi5, f4, f5).

release_forks(P, phi1) :-
	get(P, slot, f5, F5),
	send(F5, free),
	get(P, slot, f1, F1),
	send(F1, free).

release_forks(P, phi2) :-
	get(P, slot, f1, F1),
	send(F1, free),
	get(P, slot, f2, F2),
	send(F2, free).

release_forks(P, phi3) :-
	get(P, slot, f2, F2),
	send(F2, free),
	get(P, slot, f3, F3),
	send(F3, free).

release_forks(P, phi4) :-
	get(P, slot, f3, F3),
	send(F3, free),
	get(P, slot, f4, F4),
	send(F4, free).

release_forks(P, phi5) :-
	get(P, slot, f4, F4),
	send(F4, free),
	get(P, slot, f5, F5),
	send(F5, free).

check_forks(P, Phi, F1, F2) :-
	get(P, slot, F1, FF1),
	get(P, slot, F2, FF2),
	(   (get(Phi, slot, status, waiting),
	     get(FF1, slot, status, free),
	     get(FF2, slot, status, free))
	->
	     send(Phi, receive_forks),
	     send(FF1, used, right),
	     send(FF2, used, left)
	;
	     true).

:- pce_end_class.


:- pce_begin_class(philosopher , object, "eat, think or wait !").
variable(name, string, both).
variable(window, object, both).
variable(status, object, both, "eating/thinking/waiting").
variable(waiter, object, both).
variable(plate,  object, both).
variable(mytimer, timer, both).
variable(pos, point, both).
variable(side, object, both).
variable(old_text, object, both).
variable(window_stat, object, both).
variable(line_stat, number, both).
variable(stat_wait, my_stat, both).
variable(stat_eat, my_stat, both).
variable(stat_think, my_stat, both).

% méthode appelée lors de la destruction de l'objet
% On arrête d'abord le timer pour poursuivre ensuite
% sans problème (appel par le timer de ressources libérées)
unlink(P) :->
	send(P?mytimer, stop),

	get(P, status, Sta),
	stop_timer(P, Sta),
	get(P, slot, window_stat, WS),
	get(P, slot, line_stat, LS),
	get(LS, value, VLS),
	get(P, slot, name, Name),
	get(Name, value, V),
	sformat(A, 'Statistics of philosopher : ~w', [V]),
	new(Text, text(A)),
	send(Text, font, font(times, bold, 16)),
	Y is VLS * 30,
	send(WS, display, Text, point(30, Y)),

	VLS1 is VLS+1,
	get(P, slot, stat_think, ST),
	send(ST, statistics, WS, VLS1),

	VLS2 is VLS+2,
	get(P, slot, stat_eat, SE),
	send(SE, statistics, WS, VLS2),

	VLS3 is VLS+3,
	get(P, slot, stat_wait, SW),
	send(SW, statistics, WS, VLS3),

	send(P, send_super, unlink).

initialise(P, Name, Waiter, Plate, Window, Window_stat, Line_stat, Point, Side) :->
	% gtrace,
	send(P, slot, name, Name),
	send(P, slot, window, Window),
	send(P, slot, window_stat, Window_stat),
	Line is Line_stat * 5,
	send(P, slot, line_stat, Line),
	send(P, slot, waiter,Waiter),
	send(P, slot, plate,Plate),
	send(P, slot, status, thinking),
	send(P, slot, pos, Point),
	send(P, slot, side, Side),
	send(Window, display, Plate),
	send(P, slot, old_text, new(_, text(' '))),
	send(P, display_status),
	send(P, slot, stat_wait, new(_, my_stat('Waiting'))),
	send(P, slot, stat_eat, new(_, my_stat('Eating'))),
	send(P, slot, stat_think, new(_, my_stat('Thinking'))).


stop_timer(P, eating) :-
	get(P, slot, stat_eat, SE),
	send(SE, stop).

stop_timer(P, waiting) :-
	get(P, slot, stat_wait, SW),
	send(SW, stop).

stop_timer(P, thinking) :-
	get(P, slot, stat_think, ST),
	send(ST, stop).


% internal message send by the timer
my_message(P) :->
	% gtrace,
	get(P, slot, status, Status),
	next_status(P, Status).

% philosopher eating ==> thinking
next_status(P, eating) :-
	get(P, slot, waiter, Waiter),
	get(P, slot, stat_eat, SE),
	send(SE, stop),
	get(P, slot, stat_think, ST),
	send(ST, start),
	send(Waiter, give_back_forks, P),
	send(P, slot, status, thinking),
	send(P, display_status),
	get(P, plate, Plate),
	send(Plate, fill_pattern, colour(white)),
	I is random(20)+ 10,
	get(P, slot, mytimer, Timer),
	send(Timer, interval, I),
	send(Timer, start, once).

next_status(P, thinking) :-
	get(P, slot, waiter, Waiter),
	send(P, slot, status, waiting),
	send(P, display_status),
	get(P, slot, stat_think, ST),
	send(ST, stop),
	get(P, slot, stat_wait, SW),
	send(SW, start),
	send(Waiter, want_forks, P).

% send by the waiter
% philosopher can eat !
receive_forks(P) :->
	get(P, slot, stat_wait, SW),
	send(SW, stop),
	get(P, slot, stat_eat, SE),
	send(SE, start),
	send(P, slot, status, eating),
	send(P, display_status),
	get(P, plate, Plate),
	send(Plate, fill_pattern, colour(black)),
	I is random(20)+ 5,
	get(P, slot, mytimer, Timer),
	send(Timer, interval, I),
	send(Timer, start, once).

display_status(P) :->
	get(P, old_text, OT),
	free(OT),
	get(P, name, Name),
	get(Name, value, V),
	get(P, status, Status),
	choose_color(Status, Colour),
	sformat(A, '~w ~w', [V, Status]),
	get(P, window, W),
	get(P, pos, point(X, Y)),
	new(Text, text(A)),
	send(Text, font, font(times, bold, 16)),
	send(Text, colour, Colour),
	get(Text, string, Str),
	get(font(times, bold, 16), width(Str), M),
	(get(P, side, right) -> X1 is X - M; X1 = X),
	send(W, display, Text, point(X1, Y)),
	send(P, old_text, Text).


start(P) :->
	I is random(10)+ 2,
	get(P, slot, stat_think, ST),
	send(ST, start),
	send(P, mytimer, new(_, timer(I,message(P, my_message)))),
	send(P?mytimer, start, once).


choose_color(eating, colour(blue)).
choose_color(thinking, colour(green)).
choose_color(waiting, colour(red)).

:- pce_end_class.



:- pce_begin_class(disk, ellipse, "disk with color ").

initialise(P, C, R, Col) :->
        send(P, send_super, initialise, R, R),
	send(P, center, C),
	send(P, pen, 0),
	send(P, fill_pattern, Col).

change_color(P, Col) :->
	send(P, fill_pattern, Col).

:- pce_end_class.

:- pce_begin_class(my_stat , object, "statistics").
variable(name, string, both).
variable(nb, number, both).
variable(duration, real, both).
variable(start, real, both).

initialise(P, Name) :->
	send(P, name, Name),
	send(P, nb, 0),
	send(P, duration, 0.0).

start(P) :->
	get_time(T),
	send(P, slot, start, T).

stop(P) :->
	get_time(Fin),

	get(P, slot, nb, N),
	send(N, plus,1),
	send(P, slot, nb, N),

	get(P, slot, duration, D),
	get(P, slot, start, Deb),

	get(D, value, VD),
	get(Deb, value, VDeb),
	X is VD + Fin - VDeb,

	send(P, slot, duration, X).

statistics(P, W, L) :->
	get(P, nb, N),
	get(N, value, VN),
	get(P, duration, D),
	get(D, value, VD),
	get(P, name, Name),
	get(Name, value, V),
	sformat(A, '~w~tnb :~13| ~t~w~17| duration : ~t~1f~35|', [V, VN, VD]),
	new(Text, text(A)),
	send(Text, font, font(screen, roman, 14)),
	Y is L * 30,
	send(W, display, Text, point(40, Y)).

:-pce_end_class.

% forks changes of place
:- pce_begin_class(fork, line, "to help philosopphers to eat").
variable(value, number, both, "0 => 4").
variable(side, object, both), "left / right".
variable(status, object, both, "free / used").

initialise(P, Val) :->
	send_super(P, initialise),
	send(P, slot, value, Val),
	send(P, slot, status, free),
	compute(Val, free, _, PS, PE),
	send(P, start, PS),
	send(P, end, PE).

free(P) :->
	send(P, status, free),
	send(P, position).


used(P, Side) :->
	send(P, status, used),
	send(P, side, Side),
	send(P, position).

position(P) :->
	get(P, value, V),
	get(V, value, N),
	get(P, status, St),
	get(P, side, Side),
	compute(N, St, Side, PS, PE),
	send(P, start, PS),
	send(P, end, PE).


compute(N, free, _Side, point(XS,YS), point(XE,YE)) :-
	A is N * pi / 2.5 + pi / 5,
	XS is 400 + 100 * cos(A),
	YS is 400 + 100 * sin(A),
	XE is 400 + 180 * cos(A),
	YE is 400 + 180 * sin(A).


compute(N, used, left, point(XS,YS), point(XE,YE)) :-
	A is N * pi / 2.5 + pi / 5 - 2 * pi / 15,
	XS is 400 + 100 * cos(A),
	YS is 400 + 100 * sin(A),
	XE is 400 + 180 * cos(A),
	YE is 400 + 180 * sin(A).

compute(N, used, right, point(XS,YS), point(XE,YE)) :-
	A is N * pi / 2.5 + pi / 5 +  2 * pi / 15,
	XS is 400 + 100 * cos(A),
	YS is 400 + 100 * sin(A),
	XE is 400 + 180 * cos(A),
	YE is 400 + 180 * sin(A).

:- pce_end_class.
