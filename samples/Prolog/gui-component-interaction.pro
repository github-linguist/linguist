dialog('GUI_Interaction',
       [ object        :=
	   GUI_Interaction,
	 parts         :=
	   [ GUI_Interaction :=
	       dialog('Rosetta Code'),
	     Input_field     :=
	       text_item(input_field),
	     Increment       :=
	       button(increment),
	     Random          :=
	       button(random)
	   ],
	 modifications :=
	   [ Input_field := [ label  := 'Value :',
			      length := 28
			    ]
	   ],
	 layout        :=
	   [ area(Input_field,
		  area(54, 24, 251, 24)),
	     area(Increment,
		  area(54, 90, 80, 24)),
	     area(Random,
		  area(230, 90, 80, 24))
	   ],
	 behaviour     :=

	   [
	     Increment := [
			 message := message(@prolog,
					    increment,
					    Input_field )
		       ],
	     Random := [
			 message := message(@prolog,
					    my_random,
					    Input_field)
			  ],
	     Input_field := [
			 message := message(@prolog,
					    input,
					    GUI_Interaction,
					    Increment,
					    @receiver,
					    @arg1)
			  ]
	   ]

       ]).

gui_component :-
	make_dialog(S, 'GUI_Interaction'),
	send(S, open).


increment(Input) :-
	get(Input, selection, V),
	atom_number(V, Val),
	Val1 is Val + 1,
	send(Input, selection, Val1).

my_random(Input) :-
        new(D, dialog('GUI Interaction')),
        send(D, append(label(lbl,'Confirm your choice !'))),
        send(D, append(button(ok, message(D, return, ok)))),
        send(D, append(button(cancel, message(D, return, ko)))),
        send(D, default_button(ok)),
        get(D, confirm, Rval),
        free(D),
        (   Rval = ok
	->  X is random(10000),
	    send(Input, selection, X)
	).


input(Gui, Btn, Input, Selection) :-
	catch( (term_to_atom(T, Selection), number(T), send(Gui, focus, Btn)),
	       _,
	       (   send(@display, inform, 'Please type a number !'),
		   send(Input,clear))).
