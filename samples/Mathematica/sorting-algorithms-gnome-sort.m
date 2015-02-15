gnomeSort[list_]:=Module[{i=2,j=3},
While[ i<=Length[[list]],
If[ list[[i-1]]<=list[[i]],
 i=j; j++;,
 list[[i-1;;i]]=list[[i;;i-1]];i--;];
If[i==1,i=j;j++;]
]]
