# All of this is built-in
Derangements([1 .. 4]);
# [ [ 2, 1, 4, 3 ], [ 2, 3, 4, 1 ], [ 2, 4, 1, 3 ], [ 3, 1, 4, 2 ], [ 3, 4, 1, 2 ], [ 3, 4, 2, 1 ],
#   [ 4, 1, 2, 3 ], [ 4, 3, 1, 2 ], [ 4, 3, 2, 1 ] ]
Size(last);
# 9

NrDerangements([1 .. 4]);
# 9

# An implementation using formula D(n + 1) = n*(D(n) + D(n - 1))
NrDerangementsAlt_memo := [1, 0];
NrDerangementsAlt := function(n)
	if not IsBound(NrDerangementsAlt_memo[n + 1]) then
		NrDerangementsAlt_memo[n + 1] := (n - 1)*(NrDerangementsAlt(n - 1) + NrDerangementsAlt(n - 2));
	fi;
	return NrDerangementsAlt_memo[n + 1];
end;

L := List([0 .. 9]);

PrintArray(TransposedMat([L,
	List(L, n -> Size(Derangements([1 .. n]))),
	List(L, n -> NrDerangements([1 .. n])),
	List(L, NrDerangementsAlt)]));
# [ [       0,       1,       1,       1 ],
#   [       1,       0,       0,       0 ],
#   [       2,       1,       1,       1 ],
#   [       3,       2,       2,       2 ],
#   [       4,       9,       9,       9 ],
#   [       5,      44,      44,      44 ],
#   [       6,     265,     265,     265 ],
#   [       7,    1854,    1854,    1854 ],
#   [       8,   14833,   14833,   14833 ],
#   [       9,  133496,  133496,  133496 ] ]
