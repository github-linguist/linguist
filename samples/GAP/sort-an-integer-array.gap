a := [ 8, 2, 5, 9, 1, 3, 6, 7, 4 ];
# Make a copy (with "b := a;", b and a would point to the same list)
b := ShallowCopy(a);

# Sort in place
Sort(a);
a;
# [ 1, 2, 3, 4, 5, 6, 7, 8, 9 ]

# Sort without changing the argument
SortedList(b);
# [ 1, 2, 3, 4, 5, 6, 7, 8, 9 ]
b;
# [ 8, 2, 5, 9, 1, 3, 6, 7, 4 ]
