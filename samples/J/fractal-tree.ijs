require'gl2'

L0=: 50           NB. initial length
A0=: 1r8p1        NB. initial angle: pi divided by 8
dL=: 0.9          NB. shrink factor for length
dA=: 0.75         NB. shrink factor for angle
N=: 14            NB. number of branches

L=: L0*dL^1+i.N  NB. lengths of line segments

NB. relative angles of successive line segments
A=: A0*(dA^i.N) +/\@:*("1) _1 ^ #:i.2 ^ N

NB. end points for each line segment
P=: 0 0+/\@,"2 +.*.inv (L0,0),"2 L,"0"1 A

P_C_paint=: gllines_jgl2_ bind (10 + ,/"2 P-"1<./,/P)
wd 0 :0
 pc P closeok;
 xywh 0 0 250 300;
 cc C isigraph rightmove bottommove;
 pas 0 0;
 pshow;
)
