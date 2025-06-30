#include <incmode>.

% if set to one, permits consecutive moves in one time step
% this will not provide optimal plans but usually finds solutions much faster
#const consecutive = 0.

#program base.

% neighborhood relation
d(1,0;0,1;-1,0;0,-1).
n((X,Y),(X+DX,Y+DY)) :- dim((X,Y)), dim((X+DX,Y+DY)), d(DX,DY).

% positions at time step zero
at(0,(X,Y),N) :- in(X,Y,N).

#program step(t).

% guess moves
1 { move(t,A,B) : n(A,B) } 1 :- dim(B), not at(t-1,B,_).
0 { move(t,A,B) : n(A,B) } 1 :- move(t,B,_), consecutive == 1.

% check moves
 :- 2 { move(t,A,B) }, dim(A).

% state transition
at(t,A,N) :- at(t-1,A,N), not move(t,A,_).
at(t,B,N) :- at(t-1,A,N),     move(t,A,B).

% some redundant constraints
 :- move(t,A,_), not at(t-1,A,_).
 :- move(t,A,B), move(t-1,B,A), consecutive != 1.

#program check(t).

% check domain knowledge
 :- not 1 { not at(t,A,_) : dim(A) } 1.
 :- 2 { at(t,A,N) }, dim(A).

% check goal
 :- go(X,Y,N), not at(t,(X,Y),N), query(t).

#show move/3.
#show at/3.