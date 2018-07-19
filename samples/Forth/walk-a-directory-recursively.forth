defer ls-filter

: dots? ( name len -- ? )
  dup 1 = if drop c@ [char] . =
  else 2 = if dup c@ [char] . = swap 1+ c@ [char] . = and
  else drop false then then ;

: ls-r ( dir len -- )
  open-dir if drop exit then  ( dirid)
  begin
    dup pad 256 rot read-dir throw
  while
    pad over dots? 0= if   \ ignore current and parent dirs
      pad over recurse
      pad over ls-filter if
        cr pad swap type
      else drop then
    else drop then
  repeat
  drop close-dir throw ;

: c-file? ( str len -- ? )
  dup 3 < if 2drop false exit then
  + 1- dup c@ 32 or
   dup [char] c <> swap [char] h <> and if drop false exit then
  1- dup c@ [char] . <> if drop false exit then
  drop true ;
' c-file? is ls-filter

s" ." ls-r
