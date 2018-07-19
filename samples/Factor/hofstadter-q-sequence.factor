( scratchpad ) : next ( seq -- newseq )
dup 2 tail* over length [ swap - ] curry map
[ dupd swap nth ] map 0 [ + ] reduce suffix ;

( scratchpad ) { 1 1 } 1000 [ next ] times  dup 10 head .  999 swap nth .
{ 1 1 2 3 3 4 5 5 6 6 }
502
