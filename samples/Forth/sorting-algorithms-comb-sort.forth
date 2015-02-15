defer precedes
defer exchange

: gnomesort                            ( a n)
  swap >r 1                            ( n c)
  begin                                ( n c)
    over over >                        ( n c f)
  while                                ( n c)
    dup if                             ( n c)
      dup dup 1- over over r@ precedes
      if r@ exchange 1- else drop drop 1+ then
    else 1+ then                       ( n c)
  repeat drop drop r> drop             ( --)
;

: combsort                             ( a n --)
  dup begin                            ( a n g)
    10 13 */ tuck >r >r 0              ( a g 0)
    begin                              ( a g 0)
      over r@ <                        ( a g 0 f)
    while                              ( a g 0)
      rot >r over over r@ precedes     ( g 0 f)
      if over over r@ exchange then    ( g 0)
      r> rot 1+ rot 1+                 ( a g 0)
    repeat drop drop r> r>             ( a n g)
    dup 9 <                            ( a n g f)
  until drop gnomesort                 ( --)
;

create example
  8 93 69 52 50 79 33 52 19 77 , , , , , , , , , ,
 72 85 11 61 64 80 64 76 47 65 , , , , , , , , , ,
  13 47 23 40 87 45 2 48 22 69 , , , , , , , , , ,
  1 53 33 60 57 14 76 32 59 12 , , , , , , , , , ,
 74 38 39 22 87 28 37 93 71 88 , , , , , , , , , ,
 56 35 48 99 21 35 26 28 58 85 , , , , , , , , , ,
 27 16 54 88 82 18 45 64 45 87 , , , , , , , , , ,
   98 97 60 77 43 1 64 0 32 89 , , , , , , , , , ,
  77 90 68 83 9 76 10 10 95 12 , , , , , , , , , ,
   99 23 74 58 54 25 50 9 94 1 , , , , , , , , , ,

:noname >r cells r@ + @ swap cells r> + @ swap < ; is precedes
:noname >r cells r@ + swap cells r> + over @ over @ swap rot ! swap ! ; is exchange

: .array 100 0 do example i cells + ? loop cr ;

.array example 100 combsort .array
