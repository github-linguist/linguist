data = Import["http://www.puzzlers.org/pub/wordlists/unixdict.txt", "List"];
result = DeleteDuplicates[ Select[data, MemberQ[data, StringReverse[#]] && # =!= StringReverse[#] &], (# ===StringReverse[#2]) &];
Print[Length[result], Take[result, 5]]
