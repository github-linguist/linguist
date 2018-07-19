require tasker.fs
require random.fs

: task ( str len -- )
  64 NewTask 2 swap pass
  ( str len -- )
  10 0 do
    100 random ms
    pause 2dup cr type
  loop 2drop ;

: main
  s" Enjoy"   task
  s" Rosetta" task
  s" Code"    task
  begin pause single-tasking? until ;
main
