include ffl/hct.fs
include ffl/hci.fs

\ Create hashtable and iterator in dictionary
10     hct-create htable
htable hci-create hiter

\ Insert entries
1 s" hello" htable hct-insert
2 s" world" htable hct-insert
3 s" !"     htable hct-insert

: iterate
  hiter hci-first
  BEGIN
  WHILE
    ." key = " hiter hci-key type ." , value = " . cr
    hiter hci-next
  REPEAT
;

iterate
