\ grayscale bitmap (without word-alignment for scan lines)

\ bdim, bwidth, bdata all work with graymaps

: graymap ( w h -- gmp )
  2dup * bdata allocate throw
  dup >r 2! r> ;

: gxy ( x y gmp -- addr )
  dup bwidth rot * rot + swap bdata + ;

: g@ ( x y gmp -- c ) gxy c@ ;
: g! ( c x y bmp -- ) gxy c! ;

: gfill ( c gmp -- )
  dup bdata swap bdim * rot fill ;

: gshow ( gmp -- )
  dup bdim
  0 do cr
    dup 0 do
      over i j rot g@ if [char] * emit else space then
    loop
  loop
  2drop ;

\ RGB <-> Grayscale
: lum>rgb ( 0..255 -- pixel )
   dup 8 lshift or
   dup 8 lshift or ;

: pixel>rgb ( pixel -- r g b )
  256 /mod 256 /mod ;
: rgb>lum ( pixel -- 0..255 )
  pixel>rgb
   722 *   swap
  7152 * + swap
  2126 * + 10000 / ;

: bitmap>graymap ( bmp -- gmp )
  dup bdim graymap
  dup bdim nip 0 do
    dup bwidth 0 do
      over i j rot b@ rgb>lum
      over i j rot g!
    loop
  loop nip ;

: graymap>bitmap ( gmp -- bmp )
  dup bdim bitmap
  dup bdim nip 0 do
    dup bwidth 0 do
      over i j rot g@ lum>rgb
      over i j rot b!
    loop
  loop nip ;
