\ binary tree (dictionary)
: node ( l r data -- node ) here >r , , , r> ;
: leaf ( data -- node ) 0 0 rot node ;

: >data  ( node -- ) @ ;
: >right ( node -- ) cell+ @ ;
: >left  ( node -- ) cell+ cell+ @ ;

: preorder ( xt tree -- )
  dup 0= if 2drop exit then
  2dup >data swap execute
  2dup >left recurse
       >right recurse ;

: inorder ( xt tree -- )
  dup 0= if 2drop exit then
  2dup >left recurse
  2dup >data swap execute
       >right recurse ;

: postorder ( xt tree -- )
  dup 0= if 2drop exit then
  2dup >left recurse
  2dup >right recurse
       >data swap execute ;

: max-depth ( tree -- n )
  dup 0= if exit then
  dup  >left recurse
  swap >right recurse max 1+ ;

defer depthaction
: depthorder ( depth tree -- )
  dup 0= if 2drop exit then
  over 0=
  if   >data depthaction drop
  else over 1- over >left  recurse
       swap 1- swap >right recurse
  then ;

: levelorder ( xt tree -- )
  swap is depthaction
  dup max-depth 0 ?do
    i over depthorder
  loop drop ;

7 leaf 0      4 node
              5 leaf 2 node
8 leaf 9 leaf 6 node
              0      3 node 1 node value tree

cr ' . tree preorder    \ 1 2 4 7 5 3 6 8 9
cr ' . tree inorder     \ 7 4 2 5 1 8 6 9 3
cr ' . tree postorder   \ 7 4 5 2 8 9 6 3 1
cr tree max-depth .     \ 4
cr ' . tree levelorder  \ 1 2 3 4 5 6 7 8 9
