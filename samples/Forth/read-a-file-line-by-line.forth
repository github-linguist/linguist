4096 constant max-line

: third ( A b c -- A b c A )
  >r over r> swap ;

: read-lines ( fileid -- )
  begin  pad max-line third read-line throw
  while  pad swap  ( fileid c-addr u )  \ string excludes the newline
         2drop
  repeat 2drop ;
