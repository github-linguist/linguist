FixedPartitions := function(arg)
	local aux;
	aux := function(i, u)
		local r, v, w;
		if i = Size(arg) then
			return [[u]];
		else
			r := [ ];
			for v in Combinations(u, arg[i]) do
				for w in aux(i + 1, Difference(u, v)) do
					Add(r, Concatenation([v], w));
				od;
			od;
			return r;
		fi;
	end;
	return aux(1, [1 .. Sum(arg)]);
end;


FixedPartitions(2, 0, 2);
# [ [ [ 1, 2 ], [  ], [ 3, 4 ] ], [ [ 1, 3 ], [  ], [ 2, 4 ] ],
#   [ [ 1, 4 ], [  ], [ 2, 3 ] ], [ [ 2, 3 ], [  ], [ 1, 4 ] ],
#   [ [ 2, 4 ], [  ], [ 1, 3 ] ], [ [ 3, 4 ], [  ], [ 1, 2 ] ] ]

FixedPartitions(1, 1, 1);
# [ [ [ 1 ], [ 2 ], [ 3 ] ], [ [ 1 ], [ 3 ], [ 2 ] ], [ [ 2 ], [ 1 ], [ 3 ] ],
#   [ [ 2 ], [ 3 ], [ 1 ] ], [ [ 3 ], [ 1 ], [ 2 ] ], [ [ 3 ], [ 2 ], [ 1 ] ] ]
