Soundex[ input_ ] := Module[{x = input, head, body},
{head, body} = {First@#, Rest@#}&@ToLowerCase@Characters@x;
body = (Select[body, FreeQ[Characters["aeiouyhw"],#]&] /. {("b"|"f"|"p"|"v")->1,
("c"|"g"|"j"|"k"|"q"|"s"|"x"|"z")->2, ("d"|"t")->3,"l"->4 ,("m"|"n")->5, "r"->6});
If[Length[body] < 3,
 body = PadRight[body, 3],
 body = DeleteDuplicates[body]
];
StringJoin @@ ToString /@ PrependTo[ body[[1 ;; 3]], ToUpperCase@head]]
