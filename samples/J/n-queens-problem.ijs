perm   =: ! A.&i. ]               NB. all permutations of integers 0 to y
comb2  =: (, #: I.@,@(</)&i.)~    NB. all size 2 combinations of integers 0 to y
mask   =: [ */@:~:&(|@-/) {
queenst=: comb2 (] #"1~ mask)&.|: perm
