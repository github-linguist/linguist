f2=Function[{l,k},
  Module[{n=Length@l,m},
  m=SparseArray[{{i_,j_}/;i==1||i==j+1->1},{n,n}];
  NestList[m.#&,l,k]]];
Table[Last/@f2[{1,1}~Join~Table[0,{n-2}],15+n][[-18;;]],{n,2,10}]//TableForm
Table[Last/@f2[{1,2}~Join~Table[0,{n-2}],15+n][[-18;;]],{n,2,10}]//TableForm
