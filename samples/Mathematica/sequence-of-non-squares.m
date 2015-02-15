nonsq = (# + Floor[0.5 + Sqrt[#]]) &;
nonsq@Range[22]
If[! Or @@ (IntegerQ /@ Sqrt /@ nonsq@Range[10^6]),
 Print["No squares for n <= ", 10^6]
 ]
