# Built-in, using sets (which are also lists)
a := [ 1, 2, 3, 1, [ 4 ], 5, 5, [4], 6 ];
# [ 1, 2, 3, 1, [ 4 ], 5, 5, [ 4 ], 6 ]
b := Set(a);
# [ 1, 2, 3, 5, 6, [ 4 ] ]
IsSet(b);
# true
IsList(b);
# true
