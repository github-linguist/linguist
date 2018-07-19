defer ls-filter ( name len -- ? )
: ls-all  2drop true ;
: ls-visible  drop c@ [char] . <> ;

: ls ( dir len -- )
  open-dir throw  ( dirid )
  begin
    dup pad 256 rot read-dir throw
  while
    pad over ls-filter if
      cr pad swap type
    else drop then
  repeat
  drop close-dir throw ;

\ only show C language source and header files (*.c *.h)
: c-file? ( str len -- ? )
  dup 3 < if 2drop false exit then
  + 1- dup c@
   dup [char] c <> swap [char] h <> and if drop false exit then
  1- dup c@ [char] . <> if drop false exit then
  drop true ;
' c-file? is ls-filter

s" ." ls
