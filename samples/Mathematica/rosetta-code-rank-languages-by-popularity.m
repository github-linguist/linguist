Languages = Flatten[Import["http://rosettacode.org/wiki/Category:Programming_Languages","Data"][[1,1]]];
Languages = Most@StringReplace[Languages, {" " -> "_", "+" -> "%2B"}];
b = {#, If[#  ===  {}, 0, #[[1]]]&@( StringCases[Import["http://rosettacode.org/wiki/Category:"<>#,"Plaintext"],
   "category, out of " ~~ x:NumberString ~~ " total" ->x])} &/@ Languages;
For[i = 1, i < Length@b , i++ , Print[i, ". ", #[[2]], " - ", #[[1]] ]&@ Part[Reverse@SortBy[b, Last], i]]
