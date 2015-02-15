:- dynamic click/1.

dialog('Simple windowed application',
       [ object        :=
	   Simple_windowed_application,
	 parts         :=
	   [ Simple_windowed_application :=
	       dialog('Simple windowed application'),
	     Name                       :=
	       label(name, 'There have been no clicks yet'),
	     BtnClick                     :=
	       button(button)
	   ],
	 modifications :=
	   [ BtnClick := [ label := 'Click me !'
		       ]
	   ],
	 layout        :=
	   [ area(Name,
		  area(40, 20, 200, 18)),
	     area(BtnClick,
		  area(90, 60, 80, 24))
	   ],
	 behaviour :=
	 [
	  BtnClick := [message := message(@prolog, btnclick, Name)]
	 ]
       ]).

btnclick(Label) :-
	retract(click(V)),
	V1 is V+1,
	assert(click(V1)),
	sformat(A, '~w click(s)', [V1]),
	send(Label, selection, A).

simple_windowed :-
	retractall(click(_)),
	assert(click(0)),
	make_dialog(D, 'Simple windowed application'),
	send(D, open).
