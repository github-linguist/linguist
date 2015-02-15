(*Ex1*)a = {{1, 3, 5}, {2, 4, 7}, {1, 1, 0}};
{lu, p, c} = LUDecomposition[a];
l = LowerTriangularize[lu, -1] + IdentityMatrix[Length[p]];
u = UpperTriangularize[lu];
P = Part[IdentityMatrix[Length[p]], p] ;
MatrixForm /@ {P.a , P, l, u, l.u}

(*Ex2*)a = {{11, 9, 24, 2}, {1, 5, 2, 6}, {3, 17, 18, 1}, {2, 5, 7, 1}};
{lu, p, c} = LUDecomposition[a];
l = LowerTriangularize[lu, -1] + IdentityMatrix[Length[p]];
u = UpperTriangularize[lu];
P = Part[IdentityMatrix[Length[p]], p] ;
MatrixForm /@ {P.a , P, l, u, l.u}
