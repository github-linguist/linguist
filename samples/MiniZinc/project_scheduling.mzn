int: n = 8; % no of tasks max
set of int: TASK = 1..n;
int: foundations = 1; int: interior_walls =2; int: exterior_walls = 3;
int: chimney = 4; int: roof = 5; int: doors = 6;
int: tiles = 7; int: windows = 8;
array[TASK] of int: duration =
  [7,4,3,3,2,2,3,3];
int: p = 8; % number of precedences
set of int: PREC = 1..p;
array[PREC,1..2] of TASK: pre = 
  [| foundations, interior_walls 
   | foundations, exterior_walls
   | foundations, chimney
   | exterior_walls, roof
   | exterior_walls, windows
   | interior_walls, doors
   | chimney, tiles
   | roof, tiles |];


int: t = sum(duration);
array[TASK] of var 0..t: start;

constraint forall(i in PREC)
                 (start[pre[i,1]] + duration[pre[i,1]] <= start[pre[i,2]]);

var int: makespan = max(t in TASK)(start[t] + duration[t]);

solve minimize makespan;

output [show(makespan)," = ", show(start)];