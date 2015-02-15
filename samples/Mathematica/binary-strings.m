(* String creation and destruction *)  BinaryString = {}; BinaryString = . ;
(* String assignment *)   BinaryString1 = {12,56,82,65} ,  BinaryString2 = {83,12,56,65}
-> {12,56,82,65}
-> {83,12,56,65}
(* String comparison *)   BinaryString1 === BinaryString2
-> False
(* String cloning and copying *)  BinaryString3 = BinaryString1
-> {12,56,82,65}
(* Check if a string is empty *)  BinaryString3 === {}
-> False
(* Append a byte to a string *)   AppendTo[BinaryString3, 22]
-> {12,56,82,65,22}
(* Extract a substring from a string *)  Take[BinaryString3, {2, 5}]
-> {56,82,65,22}
(* Replace every occurrence of a byte (or a string) in a string with another string *)
BinaryString3 /. {22 -> Sequence[33, 44]}
-> {12,56,82,65,33,44}
(* Join strings *)  BinaryString4 = Join[BinaryString1 , BinaryString2]
-> {12,56,82,65,83,12,56,65}
