: prime? ( n -- ? )
        dup 2 < if      drop false
    else dup 2 = if      drop true
    else dup 1 and 0= if drop false
    else 3
        begin 2dup dup * >=
        while 2dup mod 0=
              if       2drop false exit
              then 2 +
        repeat         2drop true
    then then then ;
