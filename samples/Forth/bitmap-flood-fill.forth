: third 2 pick ;
: 3dup  third third third ;
: 4dup  2over 2over ;

: flood ( color x y bmp -- )
  3dup b@ >r  ( R: color to fill )
  4dup b!
  third 0 > if
    rot 1- -rot
    3dup b@ r@ = if recurse then
    rot 1+ -rot
  then
  third 1+ over bwidth < if
    rot 1+ -rot
    3dup b@ r@ = if recurse then
    rot 1- -rot
  then
  over 0 > if
    swap 1- swap
    3dup b@ r@ = if recurse then
    swap 1+ swap
  then
  over 1+ over bheight < if
    swap 1+ swap
    3dup b@ r@ = if recurse then
    swap 1- swap
  then
  r> drop ;
