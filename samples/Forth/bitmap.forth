hex
0000ff constant red
00ff00 constant green
ff0000 constant blue
decimal

1 cells constant pixel
: pixels cells ;

: bdim ( bmp -- w h ) 2@ ;
: bheight ( bmp -- h ) @ ;
: bwidth ( bmp -- w ) bdim drop ;
: bdata ( bmp -- addr ) 2 cells + ;

: bitmap ( w h -- bmp )
  2dup * pixels bdata allocate throw
  dup >r 2! r> ;

: bfill ( pixel bmp -- )
  dup bdata swap bdim * pixels
  bounds do
    dup i !
  pixel +loop
  drop ;

: bxy ( x y bmp -- addr )
  dup >r bwidth * + pixels r> bdata + ;

: b@ ( x y bmp -- pixel ) bxy @ ;
: b! ( pixel x y bmp -- ) bxy ! ;

: bshow ( bmp -- )
  hex
  dup bdim
  0 do cr
    dup 0 do
      over i j rot b@ if [char] * else bl then emit  \ 7 u.r
    loop
  loop
  2drop decimal ;

4 3 bitmap value test
red test bfill
test bshow cr
