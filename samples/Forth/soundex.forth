: alpha-table create does> swap 32 or [char] a - 0 max 26 min + 1+ c@ ;

alpha-table soundex-code
  ,"  123 12. 22455 12623 1.2 2 "
   \ ABCDEFGHIJKLMNOPQRSTUVWXYZ

: soundex ( name len -- pad len )
  over c@ pad c!                  \ First character verbatim
  pad 1+ 3 [char] 0 fill          \ Pad to four characters with zeros
  1 pad c@ soundex-code  ( count code )
  2swap bounds do
    i c@ soundex-code   ( count code next )
    2dup = if         drop else   \ runs are ignored
    dup [char] . = if drop else   \ W, H don't separate runs of consonants
    dup bl = if        nip else   \ vowels separate consonants but aren't coded
      nip
      2dup swap pad + c!
      swap 1+
      tuck 4 = if leave then
    then then then
  loop
  2drop pad 4 ;

\ Knuth's test cases
s" Euler"       soundex cr type   \ E460
s" Gauss"       soundex cr type   \ G200
s" Hilbert"     soundex cr type   \ H416
s" Knuth"       soundex cr type   \ K530
s" Lloyd"       soundex cr type   \ L300
s" Lukasiewicz" soundex cr type   \ L222 (W test)
s" Ellery"      soundex cr type   \ E460
s" Ghosh"       soundex cr type   \ G200
s" Heilbronn"   soundex cr type   \ H416
s" Kant"        soundex cr type   \ K530
s" Ladd"        soundex cr type   \ L300
s" Lissajous"   soundex cr type   \ L222

s" Wheaton"   soundex cr type   \ W350
s" Ashcraft"  soundex cr type   \ A261  (H tests)
s" Burroughs" soundex cr type   \ B620
s" Burrows"   soundex cr type   \ B620  (W test) (any Welsh names?)
s" O'Hara"    soundex cr type   \ O600  (punctuation test)
