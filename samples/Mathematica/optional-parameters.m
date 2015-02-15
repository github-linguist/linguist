Options[OptionalSort]={ordering->lexicographic,column->1,reverse-> False};
OptionalSort[x_List,OptionsPattern[]]:=If[OptionValue[reverse]==True,
SortBy[x ,#[[OptionValue[column]]]&]//Reverse,
SortBy[x,#[[OptionValue[column]]]&] ]

OptionalSort[{{"a" ,"b", "c"}, {"", "q", "z"},{"zap" ,"zip", "Zot"}} ]
->{{,q,z},{a,b,c},{zap,zip,Zot}}

OptionalSort[{{"a" ,"b", "c"}, {"", "q", "z"},{"zap" ,"zip", "Zot"}},{ordering->lexicographic,column->2,reverse-> True} ]
->{{zap,zip,Zot},{,q,z},{a,b,c}}
