   c =: 0 10 20 30 40 NB.  A collection

   c, 50              NB.  Append 50 to the collection
0 10 20 30 40 50
   _20 _10 , c        NB.  Prepend _20 _10 to the collection
_20 _10 0 10 20 30 40

   ,~  c               NB.  Self-append
0 10 20 30 40 0 10 20 30 40
   ,:~  c              NB.  Duplicate
0 10 20 30 40
0 10 20 30 40

   30 e. c             NB.  Is 30 in the collection?
1
   30 i.~c             NB.  Where?
3
   30 80 e. c          NB.  Don't change anything to test multiple values -- collections are native.
1 0

   2 1 4 2 { c         NB.  From the collection, give me items two, one, four, and two again.
20 10 40 20

   |.c                 NB.  Reverse the collection
40 30 20 10 0
   1+c                 NB.  Increment the collection
1 11 21 31 41
   c%10                NB.  Decimate the collection (divide by 10)
0 1 2 3 4

   {. c                NB.  Give me the first item
0
   {: c                NB.  And the last
40
   3{.c                NB.  Give me the first 3 items
0 10 20
   3}.c                NB.  Throw away the first 3 items
30 40
   _3{.c               NB.  Give me the last 3 items
20 30 40
   _3}.c               NB.  (Guess)
0 10

     keys_map_  =:  'one';'two';'three'
     vals_map_  =:  'alpha';'beta';'gamma'
   lookup_map_  =:  a:& $: : (dyad def ' (keys i. y) { vals,x')&boxopen
   exists_map_  =:  verb def 'y e. keys'&boxopen

   exists_map_ 'bad key'
0
   exists_map_ 'two';'bad key'
1 0

   lookup_map_ 'one'
+-----+
|alpha|
+-----+
   lookup_map_ 'three';'one';'two';'one'
+-----+-----+----+-----+
|gamma|alpha|beta|alpha|
+-----+-----+----+-----+
   lookup_map_ 'bad key'
++
||
++
   'some other default' lookup_map_ 'bad key'
+------------------+
|some other default|
+------------------+
   'some other default' lookup_map_ 'two';'bad key'
+----+------------------+
|beta|some other default|
+----+------------------+

   +/ c                NB. Sum of collection
100
   */ c                NB.  Product of collection
0

   i.5                 NB.  Generate the first 5 nonnegative integers
0 1 2 3 4
   10*i.5              NB.  Looks familiar
0 10 20 30 40
   c = 10*i.5          NB.  Test each for equality
1 1 1 1 1
   c -: 10 i.5         NB.  Test for identicality
1
