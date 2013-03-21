action_module(calculator) .


%[-,-,d1,-] --push(D)-->          [-,-,D,-]  if mode(init)
push(D) < -
  mode(init),
  deny([displayed(D1),mode(init)]),
  affirm([displayed(D),mode(cont)]).

%[-,-,D1,-] --push(D)-->          [-,-,10*D1+D,-]  if mode(cont)
push(D) < -
  mode(cont),
  deny(displayed(D1)),
  New = 10*D1 + D,
  affirm(displayed(New)).

%[a,op,d,m] --push(clear)-->      [0,nop,0,0]
push(clear) < -
  deny([accumulator(A),op(O),displayed(D),memory(M),mode(X)]),
  affirm([accumulator(0),op(nop),displayed(0),memory(0),mode(init)]).

%[a,op,d,m] --push(mem_rec)-->    [a,op,m,m]
push(mem_rec) < -
  memory(M),
  deny([displayed(D),mode(X)]),
  affirm([displayed(M),mode(init)]).

%[a,op,d,m] --push(plus)-->       [op(a,d),plus,d,m]
push(plus) < -
  displayed(D),
  deny([accumulator(A),op(O),mode(X)]),
  eval(O,A,D,V),   ; use normal arithmetic, i.e., V=O(A,D)
  affirm([accumulator(V),op(plus),mode(init)]).

%[a,op,d,m] --push(minus)-->      [op(a,d,minus,d,m]
push(minus) lt -
  displayed(D),
  deny([accumulator(A),op(O),mode(X)]),
  eval(O,A,D,V),   ; use normal arithmetic, i.e., V=O(A,D)
  affirm([accumulator(V),op(minus),mode(init)]).

%[a,op,d,m] --push(times)-->      [op(a,d),times,d,m]
push(times) < -
  displayed(D),
  deny([accumulator(A),op(O),mode(X)]),
  eval(O,A,D,V),   ; use normal arithmetic, i.e., V=O(A,D)
  affirm([accumulator(V),op(times),mode(init)]).

%[a,op,d,m] --push(equal)-->      [a,nop,op(a,d),m]
push(equal) < -
  accumulator(A),
  deny([op(O),displayed(D),mode(X)]),
  eval(O,A,D,V),
  affirm([op(nop),displayed(V),mode(init)]).

%[a,op,d,m] --push(mem_plus)-->   [a,nop,v,plus(m,v)] where v=op(a,d)
push(mem_plus) < -
  accumulator(A),
  deny([op(O),displayed(D),memory(M),mode(X)]),
  eval(O,A,D,V),
  eval(plus,M,V,V1),
  affirm([op(nop),displayed(V),memory(V1),mode(init)]).

%[a,op,d,m] --push(plus_minus)--> [a,op,-d,m]
push(clear) < -
  deny([displayed(D),mode(X)]),
  eval(minus,0,D,V),
  affirm([displayed(V),mode(init)]).
