int: n; % number of rooms
set of int: ROOM = 1..n;
int: m; % number of rectangle/offsets
set of int: ROFF = 1..m;
array[ROFF,1..4] of int: d; % defns
set of int: ROT = 1..4;
array[ROOM,ROT] of set of ROFF: shape;
int: h; % height of roll
int: maxl; % maximum length of roll

n = 3; m = 7;
d = [| 0,0,3,4  % (xoffset,yoffset,xsize,ysize)
    | 0,1,4,3
    | 1,4,1,1
    | 3,1,1,2
    | 4,2,1,1
    | 1,0,2,1 
    | 0,0,4,3 |];
shape = [| {1,3,4}, {2,5,6}, {}, {}
         | {1,3,4}, {2,5,6}, {}, {}
         | {1}, {7}, {}, {}  |];
h = 7; maxl = 12;

array[ROOM] of var 0..maxl: x;
array[ROOM] of var 0..h: y;
array[ROOM] of var ROT: rot;

var 0..maxl: l; % length of carpet used

solve minimize l;

constraint forall(i in ROOM)(shape[i,rot[i]] != {});

constraint forall(i in ROOM)(forall(r in ROFF)
  (r in shape[i,rot[i]] -> 
  (x[i] + d[r,1] + d[r,3] <= l /\
   y[i] + d[r,2] + d[r,4] <= h)));

constraint forall(i,j in ROOM where i < j)
  (forall(r1,r2 in ROFF)
 (r1 in shape[i,rot[i]] /\ 
  r2 in shape[j,rot[j]] -> 
(x[i] + d[r1,1] + d[r1,3] <= x[j] + d[r2,1]
                   \/
 x[j] + d[r2,1] + d[r2,3] <= x[i] + d[r1,1]
                   \/
 y[i] + d[r1,2] + d[r1,4] <= y[j] + d[r2,2]
                   \/
y[j] + d[r2,2] + d[r2,4] <= y[i] + d[r1,2])
   ));

output ["l = \(l);\nx = \(x);\ny = \(y);\nrot = \(rot);\n"];