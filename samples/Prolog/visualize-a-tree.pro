% direction may be horizontal/vertical/list
display_tree(Direction) :-
	sformat(A, 'Display tree ~w', [Direction]),
	new(D, window(A)),
	send(D, size, size(350,200)),
	new(T, tree(text('Root'))),
	send(T, neighbour_gap, 10),
	new(S1, node(text('Child1'))),
	new(S2, node(text('Child2'))),
	send_list(T, son,[S1,S2]),
	new(S11, node(text('Grandchild1'))),
	new(S12, node(text('Grandchild2'))),
	send_list(S1, son, [S11, S12]),
	new(S21, node(text('Grandchild3'))),
	new(S22, node(text('Grandchild4'))),
	send_list(S2, son, [S21, S22]),
	send(T, direction, Direction),
	send(D, display, T),
	send(D, open).
