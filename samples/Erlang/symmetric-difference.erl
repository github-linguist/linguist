%% Implemented by Arjun Sunel
-module(symdiff).
-export([main/0]).

main() ->
	SetA = sets:from_list(["John","Bob","Mary","Serena"]),
	SetB = sets:from_list(["Jim","Mary","John","Bob"]),
	AUnionB = sets:union(SetA,SetB),
	AIntersectionB = sets:intersection(SetA,SetB),
	SymmDiffAB = sets:subtract(AUnionB,AIntersectionB),
	sets:to_list(SymmDiffAB).
