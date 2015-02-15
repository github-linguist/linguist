: prime? ( odd -- ? )
  3
  begin 2dup dup * >=
  while 2dup mod 0=
        if 2drop false exit
        then 2 +
  repeat   2drop true ;

: 2-exp-mod { e m -- 2^e mod m }
  1
  0 30 do
    e 1 i lshift >= if
      dup *
      e 1 i lshift and if 2* then
      m mod
    then
  -1 +loop ;

: factor-mersenne ( exponent -- factor )
  16384 over /  dup 2 < abort" Exponent too large!"
  1 do
    dup i * 2* 1+      ( q )
    dup prime? if
      dup 7 and  dup 1 = swap 7 = or if
        2dup 2-exp-mod 1 = if
          nip unloop exit
        then
      then
    then drop
  loop drop 0 ;

 929 factor-mersenne .  \ 13007
4423 factor-mersenne .  \ 0
