BestShuffle[data_] :=
 Flatten[{data,First[SortBy[
     List[#, StringLength[data]-HammingDistance[#,data]] & /@ StringJoin /@ Permutations[StringSplit[data, ""]], Last]]}]

Print[#[[1]], "," #[[2]], ",(", #[[3]], ")"] & /@  BestShuffle /@ {"abracadabra","seesaw","elk","grrrrrr","up","a"}
