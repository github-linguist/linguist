defer steep         \ noop or swap
defer ystep         \ 1+ or 1-

: line ( x0 y0 x1 y1 color bmp -- )
  { color bmp }
  rot swap
  ( x0 x1 y0 y1 )
  2dup  - abs >r
  2over - abs r> <
  if         ['] swap \ swap use of x and y
  else 2swap ['] noop
  then       is steep
  ( y0 y1 x0 x1 )
  2dup >
  if swap 2swap swap  \ ensure x1 > x0
  else    2swap
  then
  ( x0 x1 y0 y1 )
  2dup >
  if   ['] 1-
  else ['] 1+
  then is ystep
  over - abs    { y deltay }
  swap 2dup - dup { deltax }
  2/ rot 1+ rot
  ( error x1+1 x0 )
  do  color i y steep bmp b!
      deltay -
      dup 0<
      if   y ystep to y
           deltax +
      then
  loop
  drop ;

5 5 bitmap value test
0 test bfill
1 0 4 1 red test line
4 1 3 4 red test line
3 4 0 3 red test line
0 3 1 0 red test line
test bshow cr
 **
 * **
*   *
** *
  **
ok
