1> Q = fifo:new().
{fifo,[],[]}
2> fifo:empty(Q).
true
3> Q2 = fifo:push(Q,1).
{fifo,[1],[]}
4> Q3 = fifo:push(Q2,2).
{fifo,[2,1],[]}
5> fifo:empty(Q3).
false
6> fifo:pop(Q3).
{1,{fifo,[],[2]}}
7> {Popped, Q} = fifo:pop(Q2).
{1,{fifo,[],[]}}
8> fifo:pop(fifo:new()).
** exception error: 'empty fifo'
     in function  fifo:pop/1
