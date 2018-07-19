: exchange ( a1 a2 -- )
  2dup c@ swap c@ rot c! swap c! ;
: reverse ( c-addr u -- )
  1- bounds begin 2dup > while
    2dup exchange
    -1 /string
  repeat 2drop ;

s" testing" 2dup reverse type   \ gnitset
