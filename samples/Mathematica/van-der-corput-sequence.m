VanDerCorput[n_,base_:2]:=Table[
  FromDigits[{Reverse[IntegerDigits[k,base]],0},base],
{k,n}]
