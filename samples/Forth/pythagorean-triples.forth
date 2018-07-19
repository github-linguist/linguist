\ Two methods to create Pythagorean Triples
\ this code has been tested using Win32Forth and gforth

: pythag_fibo ( f1 f0 -- )
     \ Create Pythagorean Triples from 4 element Fibonacci series
     \ this is called with the first two members of a 4 element Fibonacci series
     \ Price and Burkhart have two good articles about this method
     \ "Pythagorean Tree: A New Species" and
     \ "Heron's Formula, Descartes Circles, and Pythagorean Triangles"
     \ Horadam found out how to compute Pythagorean Triples from Fibonacci series

     \ compute the two other members of the Fibonacci series and put them in
     \ local variables.  I was unable to do this with out using locals
     2DUP + 2DUP + 2OVER 2DUP + 2DUP +
     LOCALS| f3 f2 f1 f0 |

     wk_level @  9 .r f0 8 .r  f1 8 .r  f2 8 .r  f3 8 .r

     \ this block calculates the sides of the Pythagorean Triangle using single precision
     \ f0 f3 * 14 .r                 \ side a  (always odd)
     \ 2 f1 * f2 * 10 .r             \ side b  (a multiple of 4)
     \ f0 f2 * f1 f3 * + 10 .r       \ side c, the hyponenuse, (always odd)

     \ this block calculates double precision values
     f0 f3 um* 15 d.r                    \ side a  (always odd)
     2 f1 * f2 um* 15 d.r                \ side b  (a multiple of 4)
     f0 f2 um* f1 f3 um* d+ 17 d.r  cr   \ side c, the hypotenuse, (always odd)

     MAX_LEVEL @ wk_LEVEL @ U> IF   \ TRUE if MAX_LEVEL > WK_LEVEL
     wk_level @ 1+ wk_level !

     \ this creates a teranary tree of Pythagorean triples
     \ use a two of the members of the Fibonacci series as seeds for the
     \ next level
     \ It's the same tree created by Barning or Hall using matrix multiplication
     f3 f1 recurse
     f3 f2 recurse
     f0 f2 recurse

     wk_level @ 1- wk_level !

     else
     then

     drop drop drop drop ;

\ implements the Fibonacci series -- Pythagorean triple
\ the stack contents sets how many iteration levels there will be
: pf_test
     \ the stack contents set up the maximum level
     max_level !
     0 wk_level !
     cr

     \ call the function with the first two elements of the base Fibonacci series
     1 1 pythag_fibo  ;

: gcd ( a b -- gcd )
  begin ?dup while tuck mod repeat ;

\ this is the classical algorithm, known to Euclid, it is explained in many
\ books on Number Theory
\ this generates all primitive Pythagorean triples

\ i -- inner loop index or current loop index
\ j -- outer loop index
\ stack contents is the upper limit for j
\ i and j can not both be odd
\ the gcd( i, j ) must be 1
\ j is greater than i
\ the stack contains the upper limit of the j variable
: pythag_ancn  ( limit -- )
     cr
     1 + 2 do
        i 1 and if 2 else 1 then
        \ this sets the start value of the inner loop so that
        \ if the outer loop index is odd only even inner loop indices happen
        \ if the outer loop index is even only odd inner loop indices happen
        i swap do
             i j gcd 1 - 0> if else  \ do this if gcd( i, j ) is 1
             j 5 .r i 5 .r

             \ j j * i i * - 12 .r   \ a side of Pythagorean triangle (always odd)
             \ i j * 2 * 9 .r        \ b side of Pythagorean triangle (multiple of 4)
             \ i i * j j * + 9 .r    \ hypotenuse of Pythagorean triangle (always odd)

             \ this block calculates double precision Pythagorean triple values
             j j um* i i um* d- 15 d.r    \ a side of Pythagorean triangle (always odd)
             i j um* d2* 15 d.r           \ b side of Pythagorean triangle (multiple of 4)
             i i um* j j um* d+ 17 d.r    \ hypotenuse of Pythagorean triangle (always odd)

             cr then 2 +loop       \ keep i being all odd or all even
     loop ;



Current directory: C:\Forth ok
FLOAD 'C:\Forth\ancien_fibo_pythag.F'  ok
  ok

  ok
  ok
3 pf_test
        0       1       1       2       3              3              4                5
        1       3       1       4       5             15              8               17
        2       5       1       6       7             35             12               37
        3       7       1       8       9             63             16               65
        3       7       6      13      19            133            156              205
        3       5       6      11      17             85            132              157
        2       5       4       9      13             65             72               97
        3      13       4      17      21            273            136              305
        3      13       9      22      31            403            396              565
        3       5       9      14      23            115            252              277
        2       3       4       7      11             33             56               65
        3      11       4      15      19            209            120              241
        3      11       7      18      25            275            252              373
        3       3       7      10      17             51            140              149
        1       3       2       5       7             21             20               29
        2       7       2       9      11             77             36               85
        3      11       2      13      15            165             52              173
        3      11       9      20      29            319            360              481
        3       7       9      16      25            175            288              337
        2       7       5      12      17            119            120              169
        3      17       5      22      27            459            220              509
        3      17      12      29      41            697            696              985
        3       7      12      19      31            217            456              505
        2       3       5       8      13             39             80               89
        3      13       5      18      23            299            180              349
        3      13       8      21      29            377            336              505
        3       3       8      11      19             57            176              185
        1       1       2       3       5              5             12               13
        2       5       2       7       9             45             28               53
        3       9       2      11      13            117             44              125
        3       9       7      16      23            207            224              305
        3       5       7      12      19             95            168              193
        2       5       3       8      11             55             48               73
        3      11       3      14      17            187             84              205
        3      11       8      19      27            297            304              425
        3       5       8      13      21            105            208              233
        2       1       3       4       7              7             24               25
        3       7       3      10      13             91             60              109
        3       7       4      11      15            105             88              137
        3       1       4       5       9              9             40               41
 ok
  ok
10 pythag_ancn
    2    1              3              4                5
    3    2              5             12               13
    4    1             15              8               17
    4    3              7             24               25
    5    2             21             20               29
    5    4              9             40               41
    6    1             35             12               37
    6    5             11             60               61
    7    2             45             28               53
    7    4             33             56               65
    7    6             13             84               85
    8    1             63             16               65
    8    3             55             48               73
    8    5             39             80               89
    8    7             15            112              113
    9    2             77             36               85
    9    4             65             72               97
    9    8             17            144              145
   10    1             99             20              101
   10    3             91             60              109
   10    7             51            140              149
   10    9             19            180              181
 ok
