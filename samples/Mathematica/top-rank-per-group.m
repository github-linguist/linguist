InitialList ={{"Tyler Bennett","E10297",32000,"D101"},
{"John Rappl","E21437",47000,"D050"},{"George Woltman","E00127",53500,"D101"},
{"Adam Smith","E63535",18000,"D202"},{"Claire Buckman","E39876",27800,"D202"},
{"David McClellan","E04242",41500,"D101"},{"Rich Holcomb","E01234",49500,"D202"},
{"Nathan Adams","E41298",21900,"D050"},{"Richard Potter","E43128",15900,"D101"},
{"David Motsinger","E27002",19250,"D202"},{"Tim Sampair","E03033",27000,"D101"},
{"Kim Arlich","E10001",57000,"D190"},{"Timothy Grove","E16398",29900,"D190"}};

TrimmedList=Map[ If[Length[#]>3,Take[#,3],#]& ,
 Map[Reverse[SortBy[#,#[[3]]&]]&,GatherBy[InitialList,Last]]
];

Scan[((Print["Department ",#[[1,4]],"\n","Employee","\t","Id","\t","Salary"]&[#])&[#];
(Scan[Print[#[[1]],"\t",#[[2]],"\t",#[[3]]]&,#] )& [#])&,TrimmedList]
