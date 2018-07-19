: circle { x y r color bmp -- }
  1 r -  0 r 2* negate  0 r  { f ddx ddy dx dy }
  color x     y r + bmp b!
  color x     y r - bmp b!
  color x r + y     bmp b!
  color x r - y     bmp b!
  begin dx dy < while
    f 0< 0= if
      dy  1-      to dy
      ddy 2 + dup to ddy
      f +         to f
    then
    dx 1+       to dx
    ddx 2 + dup to ddx
    f 1+ +      to f
    color x dx + y dy + bmp b!
    color x dx - y dy + bmp b!
    color x dx + y dy - bmp b!
    color x dx - y dy - bmp b!
    color x dy + y dx + bmp b!
    color x dy - y dx + bmp b!
    color x dy + y dx - bmp b!
    color x dy - y dx - bmp b!
  repeat ;

12 12 bitmap value test
0 test bfill
6 6 5 blue test circle
test bshow cr
