# our deficient list
L :=
[ "ABCD", "CABD", "ACDB", "DACB", "BCDA",
  "ACBD", "ADCB", "CDAB", "DABC", "BCAD",
  "CADB", "CDBA", "CBAD", "ABDC", "ADBC",
  "BDCA", "DCBA", "BACD", "BADC", "BDAC",
  "CBDA", "DBCA", "DCAB" ];

# convert L to permutations on 1..4
u := List(L, s -> List([1..4], i -> Position("ABCD", s[i])));

# set difference (with all permutations)
v := Difference(PermutationsList([1..4]), u);

# convert back to letters
s := "ABCD";
List(v, p -> List(p, i -> s[i]));
