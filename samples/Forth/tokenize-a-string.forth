: split ( str len separator len -- tokens count )
  here >r 2swap
  begin
    2dup 2,             \ save this token ( addr len )
    2over search        \ find next separator
  while
    dup negate  here 2 cells -  +!  \ adjust last token length
    2over nip /string               \ start next search past separator
  repeat
  2drop 2drop
  r>  here over -   ( tokens length )
  dup negate allot           \ reclaim dictionary
  2 cells / ;                \ turn byte length into token count

: .tokens ( tokens count -- )
  1 ?do dup 2@ type ." ." cell+ cell+ loop 2@ type ;

s" Hello,How,Are,You,Today" s" ," split .tokens  \ Hello.How.Are.You.Today
