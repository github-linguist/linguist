: square  dup * ;
: cube  dup dup * * ;
: map. ( xt addr len -- )
  0 do  2dup i cells + @ swap execute .  loop 2drop ;

create array 1 , 2 , 3 , 4 , 5 ,
' square array 5 map. cr   \ 1 4 9 16 25
' cube   array 5 map. cr   \ 1 8 27 64 125
:noname 2* 1+ ; array 5 map. cr   \ 3 5 7 9 11
