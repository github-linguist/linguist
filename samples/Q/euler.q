/sieve:where 2=sum 0={x^/:x mod/: x} til@
/sieve:{2_where((x+1)#1b){if[x y;x[y*2+til(count[x]-y+1)div y]:0b];x}/1+1_til floor sqrt x}
sieve:{b:(x+n:1)#1b;m:floor sqrt x;while[m>=n+:1;if[b n;b[n*1+1_til x div n]:0b]];2_where b}
\
/ 1
sum where any not  til[1000] mod/: 3 5
233168

/ 2
sum {x where (x<4e6)&0=x mod 2} 40 {x,sum -2#x}/1 2
4613732

/ 3
p where 0=x mod p:sieve ceiling sqrt x:13195
last p where 0=x mod p:sieve ceiling sqrt x:600851475143
6857

/ 4
last x where {x~reverse x} each string x:asc raze x*\:/:x:100+til 900
906609

/5
{y div prd x where not 0=0N!y div x}[x]/[prd x:1+til 10]

{not all 0=y mod x}[1+til 10](1+)/1
2520
2*2*2*2*3*3*5*7*11*13*17*19
232792560

/6
(x*x:sum x)-sum x*x:1+til 10
(x*x:sum x)-sum x*x:1+til 100

/7
sieve[100] -1+6
sieve[110000] -1+10001
104743

