# Arrays are better called lists in GAP. Lists may have elements of mixed types, e$
v := [ 10, 7, "bob", true, [ "inner", 5 ] ];
# [ 10, 7, "bob", true, [ "inner", 5 ] ]

# List index runs from 1 to Size(v)
v[1];
# 10

v[0];
# error

v[5];
# [ "inner", 5 ]

v[6];
# error

# One can assign a value to an undefined element
v[6] := 100;

# Even if it's not after the last: a list may have undefined elements
v[10] := 1000;
v;
# [ 10, 7, "bob", true, [ "inner", 5 ], 100,,,, 1000 ]

# And one can check for defined values
IsBound(v[10]);
# true

IsBound(v[9]);
# false

# Size of the list
Size(v);
# 10

# Appending a list to the end of another
Append(v, [ 8, 9]);
v;
# [ 10, 7, "bob", true, [ "inner", 5 ], 100,,,, 1000, 8, 9 ]

# Adding an element at the end
Add(v, "added");
v;
# [ 10, 7, "bob", true, [ "inner", 5 ], 100,,,, 1000, 8, 9, "added" ]
