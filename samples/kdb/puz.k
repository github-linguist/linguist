/ smallest to the right that is >=
/ 9 3 5 3 7 3 7 7 9 0W
x:8 2 4 1 6 0 5 3 7 9

x?:-x:-10000

/ O(n*n)
f1:{{&/x@&x>*x}':(!#x)_\:x}
f2:{{&/y@&y>*y_:x}[x]':!#x}

r:{x?1+x:<<x} / next rank

/ O(n*log n) atom
f0:{x@{(y>)x/x y}[r x]':!#x}

/ O(n*log n) vector
g0:{x@{y x&y}[!#x]/r x}
g1:{x@{@[y;i;:;y y i:&x>y]}[!#x]/r x}
g2:{x last(*:){(#i;@[y;i;:;y y i:&x>y@:1])}[!#x]/(1;r x)}
g3:{n::r x;{n[x]:n n x@:&x>n x;x}/!#x;x n}

g0 1 4 2 6 8

a:`f1`f2`f0`g0`g1`g2`g3

1 .Q.s a!{."\\ts ",x," x"}'$a;


