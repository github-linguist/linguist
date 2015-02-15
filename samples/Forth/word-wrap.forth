\ wrap text
\ usage: gforth wrap.f in.txt 72

0. argc @ 1- arg >number 2drop drop constant maxLine

: .wrapped ( buf len -- )
  begin
    dup maxLine >
  while
    over maxLine
    begin 1- 2dup + c@ bl = until
    dup 1+ >r
    begin 1- 2dup + c@ bl <> until
    1+ type cr
    r> /string
  repeat type cr ;

: strip-nl ( buf len -- )
  bounds do
    i c@ 10 = if bl i c! then
  loop ;

argc @ 2 - arg slurp-file
2dup strip-nl
.wrapped
bye
