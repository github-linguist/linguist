: read-ppm { fid -- bmp }
  pad dup 80 fid read-line throw 0= abort" Partial line"
  s" P6" compare abort" Only P6 supported."
  pad dup 80 fid read-line throw 0= abort" Partial line"
  0. 2swap >number
  1 /string		\ skip space
  0. 2swap >number
  2drop drop nip    ( w h )
  bitmap { bmp }
  pad dup 80 fid read-line throw 0= abort" Partial line"
  s" 255" compare abort" Only 8-bits per color channel supported"
  0 pad !
  bmp bdim
  0 do
    dup 0 do
      pad 3 fid read-file throw
      3 - abort" Not enough pixel data in file"
      pad @ i j bmp b!
    loop
  loop drop
  bmp ;

\ testing round-trip
4 3 bitmap value test
red test bfill
green 1 2 test b!

s" red.ppm" w/o create-file throw
test over write-ppm
close-file throw

s" red.ppm" r/o open-file throw
dup read-ppm value test2
close-file throw

: bsize ( bmp -- len ) bdim * pixels bdata ;

test dup bsize  test2 dup bsize  compare .    \ 0 if identical
