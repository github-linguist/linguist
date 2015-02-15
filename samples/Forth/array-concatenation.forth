: $!+   ( a u a' -- a'+u )
  2dup + >r swap move r> ;
: cat   ( a2 u2 a1 u1 -- a3 u1+u2 )
  align here dup >r $!+ $!+ r> tuck - dup allot ;

\ TEST
create a1 1 , 2 , 3 ,
create a2 4 , 5 ,
a2 2 cells a1 3 cells cat dump

8018425F0: 01 00 00 00  00 00 00 00 - 02 00 00 00  00 00 00 00  ................
801842600: 03 00 00 00  00 00 00 00 - 04 00 00 00  00 00 00 00  ................
801842610: 05 00 00 00  00 00 00 00 -                           ........
