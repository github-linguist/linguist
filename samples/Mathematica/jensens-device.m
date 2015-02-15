sum[term_, i_, lo_, hi_] := Block[{temp = 0},
   				Do[temp = temp + term, {i, lo, hi}];
   				temp];
SetAttributes[sum, HoldFirst];
