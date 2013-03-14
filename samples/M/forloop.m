start1 ; entry label
 set a=1
 set b=20
 set c=2
 set sum=0
 for i=a:c do  quit:'(i<b)
 . set sum=sum+i
 . write i," : ",sum,!
 quit

start2 ; entry label
 set a=1
 set b=20
 set c=2
 set sum=0
 for i=a:c:b do
 . set sum=sum+i
 . write i," : ",sum,!
 quit
