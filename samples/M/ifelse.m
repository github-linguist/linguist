if1 ; simple if statement
 set a=5
 set b=10
 set c=25
 if (a<b) set c=b
 write c,!
 quit

if2 ; if statements contrasted
 set a=5
 set b=10
 if (a<b) write "variable a=",a," is smaller than b=",b,!
 if (a>b) write "variable a=",a," is larger  than b=",b,!
 quit

if3 ; if statement with else clause
 set a=5
 set b=10
 if (a<b) write "variable a=",a," is smaller than b=",b,!
 else  write "variable a=",a," is larger  than b=",b,!
 quit

if4 ; if statement with else clause and bodies
 set a=5
 set b=10
 set c=10
 if (a<b) do
 . write "variable a=",a," is smaller than b=",b,!
 . set c=c+a
 else  do
 . write "variable a=",a," is larger  than b=",b,!
 . set c=c+b
 write "c=",c,!
 quit

