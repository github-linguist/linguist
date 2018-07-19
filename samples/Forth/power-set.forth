: ?print dup 1 and if over args type space then ;
: .set begin dup while ?print >r 1+ r> 1 rshift repeat drop drop ;
: .powerset 0 do ." ( " 1 i .set ." )" cr loop ;
: check-none dup 2 < abort" Usage: powerset [val] .. [val]" ;
: check-size dup /cell 8 [*] >= abort" Set too large" ;
: powerset 1 argn check-none check-size 1- lshift .powerset ;

powerset
