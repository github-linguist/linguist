tree :-
	Tree= [1,
	        [2,
		   [4,
		     [7, nil, nil],
		     nil],
		   [5, nil, nil]],
	        [3,
		 [6,
		   [8, nil, nil],
		   [9,nil, nil]],
		 nil]],
	
	write('preorder    : '), preorder(Tree), nl,
	write('inorder     : '), inorder(Tree), nl,
	write('postorder   : '), postorder(Tree), nl,
	write('level-order : '), level_order([Tree]).

preorder(nil).
preorder([Node, FG, FD]) :-
	format('~w ', [Node]),
	preorder(FG),
	preorder(FD).


inorder(nil).
inorder([Node, FG, FD]) :-
	inorder(FG),
	format('~w ', [Node]),
	inorder(FD).

postorder(nil).
postorder([Node, FG, FD]) :-
	postorder(FG),
	postorder(FD),
	format('~w ', [Node]).


level_order([]).

level_order(A) :-
	level_order_(A, U-U, S),
	level_order(S).

level_order_([], S-[],S).

level_order_([[Node, FG, FD] | T], CS, FS) :-
	format('~w ', [Node]),
	append_dl(CS, [FG, FD|U]-U, CS1),
	level_order_(T, CS1, FS).

level_order_([nil | T], CS, FS) :-
	level_order_(T, CS, FS).


append_dl(X-Y, Y-Z, X-Z).
