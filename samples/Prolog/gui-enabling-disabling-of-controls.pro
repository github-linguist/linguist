dialog('GUI_Interaction',
       [ object        :=
	   GUI_Interaction,
	 parts         :=
	   [ GUI_Interaction :=
	       dialog('Rosetta Code'),
	     Name      := label(name, 'Value :'),
	     Input_field     :=
	       text_item(input_field, '0'),
	     Increment       :=
	       button(increment),
	     Decrement          :=
	       button(decrement)
	   ],
	 modifications :=
	   [ Input_field := [ label  := 'Value :',
			      length := 28,
			      show_label := @off
			    ],
	     Decrement := [active := @off]
	   ],
	 layout        :=
	   [ area(Name,
		  area(50, 26, 25, 24)),
	     area(Input_field,
		  area(95, 24, 200, 24)),
	     area(Increment,
		  area(50, 90, 80, 24)),
	     area(Decrement,
		  area(230, 90, 80, 24))
	   ],
	 behaviour     :=

	   [
	     Increment := [
			 message := message(@prolog,
					    increment,
					    Increment,
					    Decrement,
					    Input_field )
		       ],
	     Decrement := [
			 message := message(@prolog,
					    decrement,
					    Increment,
					    Decrement,
					    Input_field)
			  ],
	     Input_field := [
			 message := message(@prolog,
					    input,
					    Increment,
					    Decrement,
					    @receiver,
					    @arg1)
			  ]
	   ]

       ]).

gui_component :-
	make_dialog(S, 'GUI_Interaction'),
	send(S, open).


increment(Incr, Decr, Input) :-
	get(Input, selection, V),
	atom_number(V, Val),
	Val1 is Val + 1,
	send(Input, selection, Val1),
	test(Val1, Incr, Decr, Input).

decrement(Incr, Decr, Input) :-
	get(Input, selection, V),
	atom_number(V, Val),
	Val1 is Val - 1,
	send(Input, selection, Val1),
	test(Val1, Incr, Decr, Input).



input(Incr, Decr, Input, Selection) :-
	catch( (term_to_atom(T, Selection), number(T)),
	       _,
	       (   send(@display, inform, 'Please type a number !'),
		   T = 0,
		   send(Input,selection, T))),
	test(T, Incr, Decr, Input).


test(V, Incr, Decr, Input) :-
	(   V = 0 -> send(Input, active, @on); send(Input, active, @off)),
	send(Incr, active, @on),
	send(Decr, active, @on),
	(   V < 1
	->  send(Decr, active, @off)
	;   V > 9
	->  send(Incr, active, @off)).
