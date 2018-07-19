: str-count ( s1 len s2 len -- n )
  2swap 0 >r
  begin 2over search
  while 2over nip /string
        r> 1+ >r
  repeat 2drop 2drop r> ;

s" the three truths" s" th" str-count .    \ 3
s" ababababab" s" abab" str-count .     \ 2
