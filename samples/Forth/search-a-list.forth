include lib/row.4th

create haystack
  ," Zig"  ," Zag" ," Wally" ," Ronald" ," Bush" ," Krusty" ," Charlie"
  ," Bush" ," Boz" ," Zag" NULL ,
does>
  dup >r 1 string-key row 2>r type 2r> ."  is "
  if r> - ." at " . else r> drop drop ." not found" then cr
;

s" Washington" haystack s" Bush" haystack
