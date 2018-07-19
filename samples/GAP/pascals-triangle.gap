Pascal := function(n)
	local i, v;
	v := [1];
	for i in [1 .. n] do
		Display(v);
		v := Concatenation([0], v) + Concatenation(v, [0]);
	od;
end;

Pascal(9);
# [ 1 ]
# [ 1, 1 ]
# [ 1, 2, 1 ]
# [ 1, 3, 3, 1 ]
# [ 1, 4, 6, 4, 1 ]
# [ 1, 5, 10, 10, 5, 1 ]
# [ 1, 6, 15, 20, 15, 6, 1 ]
# [ 1, 7, 21, 35, 35, 21, 7, 1 ]
# [ 1, 8, 28, 56, 70, 56, 28, 8, 1 ]
