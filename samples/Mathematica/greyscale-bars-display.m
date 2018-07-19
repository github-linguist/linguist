CreateDocument[ Graphics[ Flatten@Table[
{ If[EvenQ[#3], GrayLevel[ 1. - j/#1 ], GrayLevel[ j/#1 ]],
   Rectangle[{j #2, 7*#3}, {#2 (j + 1), (#3 + 1) 7}]}, {j, 0, #1}] & @@@
   {{7, 8, 3}, {15, 4, 2}, {31, 2, 1}, {63, 1, 0} }
,ImageSize -> Full], WindowFrame -> "Frameless", WindowSize -> Full]
