: -leading ( addr len -- addr' len' )
  begin over c@ bl = while 1 /string repeat ;
\ -trailing is built in

s"   test  "
2dup -leading cr type
2dup -trailing cr type
     -leading -trailing cr type
