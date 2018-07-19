SetAttributes[CheckDistribution, HoldFirst]
CheckDistribution[function_,number_,delta_] :=(Print["Expected: ", N[number/7], ", Generated :",
Transpose[Tally[Table[function, {number}]]][[2]] // Sort];  If[(Max[#]-Min[#])&
 [Transpose[Tally[Table[function, {number}]]][[2]]] < delta*number/700, "Flat", "Skewed"])
