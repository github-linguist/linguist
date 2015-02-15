4096 constant max-line
: read-lines
  begin  stdin pad max-line read-line throw
  while  pad swap   \ addr len is the line of data, excluding newline
         2drop
  repeat ;
