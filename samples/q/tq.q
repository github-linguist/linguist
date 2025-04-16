dst:`:tq
src:`:tqsrc
F:key src

/ trade fields (types;widths)   trf after 200609
tf:`time`ex`sym`s`cond`size`price`stop`corr`seq`cts`trf
tt:("TCSS*IFBIJCC ";9 1 6 10 4 9 11 1 2 16 1 1,1+20060930<"I"$-8#string first F)

/ quote fields (types;widths)
qf:`time`ex`sym`s`bid`bsize`ask`asize`cond`mmid`bex`aex`seq`bbo`qbbo`corr`cqs
qt:("TCSSFIFIC*CCJCCCC ";9 1 6 10 11 7 11 7 1 4 1 1 16 1 1 1 1 2)

/ sym[.s] "e"$pricebidask 
g:{[f;x]`sym`time xcols delete s from @[;`sym;{$[null y;x;` sv x,y]}';x`s]@[x;f;"e"$%;1e4]}
foo:{[d;f;t;g;x]@[;`sym;`p#].Q.dsftg[(dst;"D"$-8#string x;d);(` sv src,x;sum t 1;0);f;t;g]}

\t foo[`trade;tf;tt;g[`price]  ]each F where F like"taqtrade*[0-9]";
\t foo[`quote;qf;qt;g[`bid`ask]]each F where F like"taqquote*[0-9]";

\
http://www.nyxdata.com/Data-Products/Daily-TAQ
