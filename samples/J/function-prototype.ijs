   NB. j assumes an unknown name f is a verb of infinite rank
   NB. f has infinite ranks
   f b. 0
_ _ _

   NB. The verb g makes a table.
   g=: f/~

   NB. * has rank 0
   f=: *


   NB. indeed, make a multiplication table
   f/~ i.5
0 0 0  0  0
0 1 2  3  4
0 2 4  6  8
0 3 6  9 12
0 4 8 12 16

   NB. g was defined as if f had infinite rank.
   g i.5
0 1 4 9 16

   NB. f is known to have rank 0.
   g=: f/~

   NB. Now we reproduce the table
   g i.5
0 0 0  0  0
0 1 2  3  4
0 2 4  6  8
0 3 6  9 12
0 4 8 12 16



   NB. change f to another rank 0 verb
   f=: +

   NB. and construct an addition table
   g i.5
0 1 2 3 4
1 2 3 4 5
2 3 4 5 6
3 4 5 6 7
4 5 6 7 8


   NB. f is multiplication at infinite rank
   f=: *"_


   NB. g, however, has rank 0
   g i.5
0 0 0  0  0
0 1 2  3  4
0 2 4  6  8
0 3 6  9 12
0 4 8 12 16
