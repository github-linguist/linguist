/
k:50;m:_(155%18)*n:1800000;o:{m:*x;$[1=#x;,"i"$!m;(,&m#n),(m*n:#*n)#'n:o 1_x]}
\t r:([]u:(n,k)#"asd";r:("i"$!n)div-_-n%2700)
\t u:([]d:`s#1973.01.01+("i"$!m)div-_-m%11574;s:m?0x0/:'+"x"$o 30 35 48 48;u:`r!m?"i"$n;a:m?1.)
{.[`:uq0;x,`;:;. x]}'`r`u
\t u:([]d:`s#1970.01.01+("i"$!m)div-_-m%15461;s:m?0i;u:`r!m?"i"$n;a:m?1.)
{.[`:uq1;x,`;:;. x]}'`r`u
\

/ 0-sigmod 1,2,3 amplab a,b,c
f:{$[n;x@.:':n#,y;. y]};."\\l uq",$~~i:3
n:."\\s"

top:{(!x)[i],d i:a?|/a:*+d:. x}
\ts f[,/]"select u,r from r where r<",$5 1 100 2699 i
\

\ts f[+/]"select sum a by s div ",($65536 2048 128 16 i)," from u"
/\t top select sum a,avg r by s from 
\ts f[,/]"select s,a,u.r from u where d within 1980.01.01 ",$-1+1980.01.08 1980.04.01 1983.01.01 2010.01.01 i

\\
