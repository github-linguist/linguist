words=First/@Import["http://www.puzzlers.org/pub/wordlists/unixdict.txt","Table"];
anagramDegrangement=Function[{w1,w2},
	Module[{c1=ToCharacterCode@w1,c2=ToCharacterCode@w2},
	Sort@c1==Sort@c2&&Select[c1-c2,#==0&,1]==={}]];
gs=Select[GatherBy[words,{StringLength@#,Union@ToCharacterCode@#}&],Length@#>=2&];
First@Flatten[Function[ws,Select[Join@@Outer[List,ws,ws,1],anagramDegrangement@@#&]]/@SortBy[gs,-StringLength@First@#&],1]
