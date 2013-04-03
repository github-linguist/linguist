start ; compute the Fibonacci series
 set (a,b)=1
 for i=1:1 do  quit:term>100
 . set term=a+b
 . write !,term
 . set a=b
 . set b=term
 write !,"Result= ",term,!
 quit
