: .filesize ( addr len -- ) 2dup type ."  is "
  r/o open-file throw
  dup file-size throw  <# #s #> type ."  bytes long." cr
  close-file throw ;

 s" input.txt" .filesize
s" /input.txt" .filesize
