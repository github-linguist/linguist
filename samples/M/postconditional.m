 ;
 ; M code examples contrasting postconditionals with IF-commands
 ;
post1 ; postconditional in set command
 set a=5
 set b=10
 set c=25
 I 0  ;purposely set $TEST to false
 write "$TEST special variable (before post-condition)=",$TEST
 set:(a<b) c=b
 write "$TEST special variable (after post-condition) =",$TEST 
 write "c =",c,!
 quit
 ;
post2 ; postconditional in write command
 set a=5
 set b=10
 I 0  ;purposely set $TEST to false
 write "$TEST special variable (before post-condition)=",$TEST
 write:(a<b) "variable a=",a," is smaller than b=",b,!
 write "$TEST special variable (after post-condition) =",$TEST 
 write:(a>b) "variable a=",a," is larger  than b=",b,!
 write "$TEST special variable (after post-condition) =",$TEST 
 quit
 ;
if1 ; if command
 set a=5
 set b=10
 set c=25
 I 0  ;purposely set $TEST to false
 write "$TEST special variable (before IF)=",$TEST
 if (a<b) set c=b
 write "$TEST special variable (after IF) =",$TEST
 write c,!
 quit
 ;
if2 ; postconditional in write command
 set a=5
 set b=10
 I 0  ;purposely set $TEST to false
 write "$TEST special variable (before IF)=",$TEST
 if (a<b) write "variable a=",a," is smaller than b=",b,!
 write "$TEST special variable (after IF) =",$TEST 
 if (a>b) write "variable a=",a," is larger  than b=",b,!
 write "$TEST special variable (after IF) =",$TEST 
 quit
