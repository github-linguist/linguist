bullCow={Count[#1-#2,0],Length[#1\[Intersection]#2]-Count[#1-#2,0]}&;
Module[{r,input,candidates=Permutations[Range[9],{4}]},
While[True,
	r=InputString[];
	If[r===$Canceled,Break[],
		input=ToExpression/@StringSplit@r;
		If[Length@input!=3,Print["Input the guess, number of bulls, number of cows, delimited by space."],
			candidates=Select[candidates,bullCow[ToCharacterCode@StringJoin[ToString/@#],ToCharacterCode@ToString@input[[1]]]==input[[2;;3]]&];
			candidates=SortBy[candidates,{-3,-1}.bullCow[ToCharacterCode@StringJoin[ToString/@#],ToCharacterCode@ToString@input[[1]]]&];
			If[candidates==={},Print["No more candidates."];Break[]];
			If[Length@candidates==1,Print["Must be: "<>StringJoin[ToString/@candidates[[1]]]];Break[]];
			Print[ToString@Length@candidates<>" candidates remaining."];
			Print["Can try "<>StringJoin[ToString/@First@candidates]<>"."];
	]]]]
