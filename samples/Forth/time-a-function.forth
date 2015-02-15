: time: ( "word" -- )
  utime 2>R ' EXECUTE
  utime 2R> D-
  <# # # # # # # [CHAR] . HOLD #S #> TYPE ."  seconds" ;

1000 time: MS  \ 1.000081 seconds ok
