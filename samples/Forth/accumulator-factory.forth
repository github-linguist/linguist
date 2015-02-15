: accumulator
  create ( n -- ) ,
  does> ( n -- acc+n ) tuck +! @ ;

0 accumulator foo

1 foo .  \ 1
2 foo .  \ 3
3 foo .  \ 6
