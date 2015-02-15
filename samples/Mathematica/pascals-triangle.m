Column[StringReplace[ToString /@ Replace[MatrixExp[SparseArray[
{Band[{2,1}] -> Range[n-1]},{n,n}]],{x__,0..}->{x},2] ,{"{"|"}"|","->" "}], Center]
