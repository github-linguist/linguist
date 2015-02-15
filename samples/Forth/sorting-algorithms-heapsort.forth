create example
  70 , 61 , 63 , 37 , 63 , 25 , 46 , 92 , 38 , 87 ,

[UNDEFINED] r'@ [IF]
: r'@ r> r> r@ swap >r swap >r ;
[THEN]

defer precedes                         ( n1 n2 a -- f)
defer exchange                         ( n1 n2 a --)

: siftDown                             ( a e s -- a e s)
  swap >r swap >r dup                  ( s r)
  begin                                ( s r)
    dup 2* 1+ dup r'@ <                ( s r c f)
  while                                ( s r c)
    dup 1+ dup r'@ <                   ( s r c c+1 f)
    if                                 ( s r c c+1)
      over over r@ precedes if swap then
    then drop                          ( s r c)
    over over r@ precedes              ( s r c f)
  while                                ( s r c)
    tuck r@ exchange                   ( s r)
  repeat then                          ( s r)
  drop drop r> swap r> swap            ( a e s)
;

: heapsort                             ( a n --)
  over >r                              ( a n)
  dup 1- 1- 2/                         ( a c s)
  begin                                ( a c s)
    dup 0< 0=                          ( a c s f)
  while                                ( a c s)
    siftDown 1-                        ( a c s)
  repeat drop                          ( a c)

  1- 0                                 ( a e 0)
  begin                                ( a e 0)
    over 0>                            ( a e 0 f)
  while                                ( a e 0)
    over over r@ exchange              ( a e 0)
    siftDown swap 1- swap              ( a e 0)
  repeat                               ( a e 0)
  drop drop drop r> drop
;

:noname >r cells r@ + @ swap cells r> + @ swap < ; is precedes
:noname >r cells r@ + swap cells r> + over @ over @ swap rot ! swap ! ; is exchange

: .array 10 0 do example i cells + ? loop cr ;

.array example 10 heapsort .array
