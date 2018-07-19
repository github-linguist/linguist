-28 constant SIGINT

: numbers ( n -- n' )
  begin dup . cr  1+  500 ms again ;

: main
  utime
  0 begin
    ['] numbers catch
    SIGINT =
  until drop
  utime d- dnegate
  <# # # # # # # [char] . hold #s #> type ."  seconds" ;

main bye
