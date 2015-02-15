   1                          NB. a stand-alone scalar value is an array without any axis
1
   NB. invoking any array produces that array as the result
   {. array=: 1 3, 6#0        NB. create, name, then get head item of the array: 1 3 0 0 0 0 0 0
1
   0 { array                  NB. another way to get the head item
1
   aword=: 'there'            NB. a literal array
   0 1 3 2 2 { aword          NB. multiple items can be drawn in a single action
three
   ]twoD=: 3 5 $ 'abcdefghijklmnopqrstuvwxyz'
abcde
fghij
klmno
   1 { twoD                   NB. item 1 from twoD - a list of three items
fghij
   1 {"1 twoD                 NB. item 1 from each rank-1 item of twoD (i.e. column 1)
bgl
   (<2 2){ twoD               NB. bracket indexing is not used in J
m
   'X' 1} aword               NB. amend item 1
tXere
   aword=: 'X' 1 4} aword     NB. in-place amend of items 1 and 4
tXerX
   'X' (0 0;1 1;2 2)} twoD    NB. amend specified items
Xbcde
fXhij
klXno
