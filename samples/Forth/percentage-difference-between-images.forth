: pixel-diff ( pixel1 pixel2 -- n )
  over 255 and over 255 and - abs >r 8 rshift swap 8 rshift
  over 255 and over 255 and - abs >r 8 rshift swap 8 rshift
                            - abs r> + r> + ;
: bdiff ( bmp1 bmp2 -- fdiff )
  2dup bdim rot bdim d<> abort" images not comparable"
  0e               ( F: total diff   )
  dup bdim * >r    ( R: total pixels )
  bdata swap bdata
  r@ 0 do
    over @ over @ pixel-diff 0 d>f f+
    cell+ swap cell+
  loop 2drop
  r> 3 * 255 * 0 d>f f/ ;

: .bdiff ( bmp1 bmp2 -- )
  cr bdiff 100e f* f. ." percent different" ;
