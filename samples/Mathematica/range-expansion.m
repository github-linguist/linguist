rangeexpand[ rng_ ] := Module[ { step1 },
step1 = StringSplit[StringReplacePart[rng,"S",StringPosition[ rng,DigitCharacter~~"-"] /. {x_,y_} -> {y,y}],","];
Flatten@ToExpression/@Quiet@StringReplace[step1,x__~~"S"~~y__->"Range["<>x<>","<>y<>"]"] ]
