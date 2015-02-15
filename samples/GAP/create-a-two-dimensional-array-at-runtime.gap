# Creating an array of 0
a := NullMat(2, 2);
# [ [ 0, 0 ], [ 0, 0 ] ]

# Some assignments
a[1][1] := 4;
a[1][2] := 5;
a[2][1] := 3;
a[2][2] := 4;

a
# [ [ 4, 5 ], [ 3, 4 ] ]

Determinant(a);
# 1
