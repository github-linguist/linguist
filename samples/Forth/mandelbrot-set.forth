500 value max-iter

: mandel ( gmp  F: imin imax rmin rmax -- )
  0e 0e { F: imin F: imax F: rmin F: rmax F: Zr F: Zi }
  dup bheight 0 do
    i s>f dup bheight s>f f/ imax imin f- f* imin f+ TO Zi
    dup bwidth 0 do
      i s>f dup bwidth s>f f/ rmax rmin f- f* rmin f+ TO Zr
      Zr Zi max-iter
      begin  1- dup
      while  fover fdup f* fover fdup f*
             fover fover f+ 4e f<
      while  f- Zr f+
             frot frot f* 2e f* Zi f+
      repeat fdrop fdrop
             drop 0        \ for a pretty grayscale image, replace with: 255 max-iter */
      else   drop 255
      then   fdrop fdrop
      over i j rot g!
    loop
  loop    drop ;

80 24 graymap
dup -1e 1e -2e 1e mandel
dup gshow
free bye
