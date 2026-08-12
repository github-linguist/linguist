% initial situation
on(D,P,0) :- init_on(D,P).

% check goal situation
ngoal(T) :- on(D,P,T), not goal_on(D,P).
:- ngoal(n).

% state transition and state constraints
1 { move(D,P,T) : disk(D), peg(P) } 1 :- ngoal(T-1), T<=n.

move(D,T)        :- move(D,P,T).
on(D,P,T)        :- move(D,P,T).
on(D,P,T)        :- on(D,P,T-1), not move(D,T), T<=n.
blocked(D-1,P,T) :- on(D,P,T-1).
blocked(D-1,P,T) :- blocked(D,P,T), disk(D).

:- move(D,P,T), blocked(D-1,P,T).
:- move(D,T), on(D,P,T-1), blocked(D,P,T).
:- disk(D), not 1 { on(D,P,T) } 1, T=1..n.

_minimize(1,T) :- ngoal(T).
