# Return the list L after applying Knuth shuffle. GAP also has the function Shuffle, which does the same.
ShuffleAlt := function(a)
   local i, j, n, t;
   n := Length(a);
   for i in [n, n - 1 .. 2] do
      j := Random(1, i);
      t := a[i];
      a[i] := a[j];
      a[j] := t;
   od;
   return a;
end;

# Return a "Permutation" object (a permutation of 1 .. n).
# They are printed in GAP, in cycle decomposition form.
PermShuffle := n -> PermListList([1 .. n], Shuffle([1 .. n]));

ShuffleAlt([1 .. 10]);
# [ 4, 7, 1, 5, 8, 2, 6, 9, 10, 3 ]

PermShuffle(10);
# (1,9)(2,3,6,4,5,10,8,7)

# One may also call the built-in random generator on the symmetric group :
Random(SymmetricGroup(10));
(1,8,2,5,9,6)(3,4,10,7)
