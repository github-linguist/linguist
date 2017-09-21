m:asc -60?`3 / mmid's
s:asc -10?`4 / syms
n:100000
B:()!() / initial bidbook

/ mmid  (time;sym;mmid;price;size)
t:flip`t`s`m`p`z!(09:30:00.0+til n;n?s;n?m;1.0*n?60;til n) /data
u:{$[(s:x`s)in key B;B[s],:x _`s;B[s]:update`g#m from enlist x _`s];} /update
q:{m:asc distinct(x:B x)`m;select sum z by p from x asof([]m;t:y)}	/query

/ orders (add,mod,del  -> price/size deltas)  bid/ask done separately
b:([]s:();p:())!() / initial sizes
t:flip`t`s`p`z!(09:30:00.0+til n;n?s;1.0*n?60;-2+n?5)
u:{x[`z]:b[`s`p#x]+:x`z;$[(s:x`s)in key B;B[s],:x _`s;B[s]:update`g#p from enlist x _`s];}
q:{p:asc distinct(x:B x)`p;select from([]p)!x asof([]p;t:y)where z>0}

\t u each t  / 100,000 updates
x:s 0;y:09:30:10.0
q[x;y]
\t do[10000;q[x;y]]

\
2006
nqds level2 100,000,000per day  10 to 100 mmid's per sym
lava orders 200,000,000per day  10 to 100 price's per sym

b,:r / nqds: select sum z by p from b where s=x
b+:r / lava: select from b where s=x,z>0
