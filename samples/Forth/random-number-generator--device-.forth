variable rnd

: randoms ( n -- )
  s" /dev/random" r/o open-file throw
  swap 0 do
    dup rnd 1 cells rot read-file throw drop
    rnd @ .
  loop
  close-file throw ;
