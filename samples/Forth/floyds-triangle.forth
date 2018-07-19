: lastn ( rows -- n ) dup 1- * 2/ ;
: width ( n -- n )  s>f flog ftrunc f>s 2 + ;

: triangle ( rows -- )
  dup lastn 0 rot ( last 0 rows )
  0 do
    over cr
    i 1+ 0 do
      1+ swap 1+ swap
      2dup width u.r
    loop
    drop
  loop
  2drop ;
