target = "METHINKS IT IS LIKE A WEASEL";
alphabet = CharacterRange["A", "Z"]~Join~{" "};
fitness = HammingDistance[target, #] &;
Mutate[parent_String, rate_: 0.01, fertility_Integer: 25] := Module[
   {offspring, kidfits, gen = 0, alphabet = CharacterRange["A", "Z"]~Join~{" "}},
   offspring = ConstantArray[Characters[parent], fertility];
   Table[
    If[RandomReal[] <= rate, offspring[[j, k]] = RandomChoice[alphabet]],
    {j, fertility}, {k, StringLength@parent}
    ];
   offspring = StringJoin[#] & /@ offspring;
   kidfits = fitness[#] & /@ Flatten[{offspring, parent}];
   Return[offspring[[First@Ordering[kidfits]]]];
   ];

mutationRate = 0.02;
parent = StringJoin[ alphabet[[RandomInteger[{1, Length@alphabet}, StringLength@target]]] ];
results = NestWhileList[Mutate[#, mutationRate, 100] &, parent, fitness[#] > 0 &];
fits = fitness[#] & /@ results;
results = Transpose[{results, fits}];
TableForm[results[[;; ;; 2]], TableHeadings->{Range[1, Length@results, 2],{"String","Fitness"}}, TableSpacing -> {1, 2}]
