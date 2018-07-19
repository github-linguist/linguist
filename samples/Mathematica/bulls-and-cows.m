digits=Last@FixedPointList[If[Length@Union@#==4,#,Table[Random[Integer,{1,9}],{4}]]&,{}]
codes=ToCharacterCode[StringJoin[ToString/@digits]];
Module[{r,bulls,cows},
	While[True,
	r=InputString[];
	If[r===$Canceled,Break[],
		With[{userCodes=ToCharacterCode@r},
		If[userCodes===codes,Print[r<>": You got it!"];Break[],
			If[Length@userCodes==Length@codes,
				bulls=Count[userCodes-codes,0];cows=Length@Intersection[codes,userCodes]-bulls;
				Print[r<>": "<>ToString[bulls]<>"bull(s), "<>ToString@cows<>"cow(s)."],
				Print["Guess four digits."]]]]]]]
