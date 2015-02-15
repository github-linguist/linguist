# According to section 21.18 of the reference manual, Sort is not stable (it's a Shell sort).
# However, SortingPerm is stable. We will see it on an example, showing indexes of elements after the sort.

n := 20;
L := List([1 .. n], i -> Random("AB"));
# "AABABBBABBABAABABBAB"


p := SortingPerm(L);
# (3,10,15,17,18,19,9,14,7,13,6,12,16,8,4)(5,11)

a := Permuted(L, p);;
b := Permuted([1 .. n], p);;

PrintArray(TransposedMat(List([1 .. n], i -> [a[i], b[i]])));
# [ [  'A',  'A',  'A',  'A',  'A',  'A',  'A',  'A',  'A',  'B',  'B',  'B',  'B',  'B',  'B',  'B',  'B',  'B',  'B',  'B' ],
#   [    1,    2,    4,    8,   11,   13,   14,   16,   19,    3,    5,    6,    7,    9,   10,   12,   15,   17,   18,   20 ] ]
