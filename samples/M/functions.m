; This function computes a factorial


factorial(n) ;;
 new f
 set f=n
 for  do  quit:n'>1
 . set n=n-1
 . set f=f*n
 . write n," : ",f,!
 quit f



main() ;;
 set x=5
 set y=$$factorial(x)
 write "Factorial of ",x," = ",y,!
 quit
