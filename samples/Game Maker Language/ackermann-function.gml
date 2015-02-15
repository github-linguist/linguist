m=argument0
n=argument1
if(m=0)
    return (n+1)
else if(n=0)
    return (ackermann(m-1,1,1))
else
    return (ackermann(m-1,ackermann(m,n-1,2),1))
