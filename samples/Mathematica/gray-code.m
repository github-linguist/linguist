graycode[n_]:=BitXor[n,BitShiftRight[n]]
graydecode[n_]:=Fold[BitXor,0,FixedPointList[BitShiftRight,n]]
