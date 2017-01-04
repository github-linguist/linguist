int: n;
set of int: W = 1..n;
int: m;
set of int: T = 1..2*m;
array[W,T] of int: profit;
array[W,W] of bool: compatible;

array[W] of var T: task;
array[T] of var opt W: worker;

constraint inverse(task,worker);
constraint forall(t in T where t mod m != 0)
                 (compatible[worker[t],worker[t+1]]);
           

solve maximize sum(w in W)(profit[w,task[w]]);

output [ if fix(occurs(worker[t])) then show(worker[t]) else " " endif ++ " "
       | t in T ] 
       ++ [ show(task), "\n" ]
       ;

predicate inverse(array[int] of var opt int: f, array[int] of var opt int: g) = 
          forall(i in index_set(f), j in index_set(g))
                (occurs(f[i]) /\ occurs(g[j]) ->
                 (deopt(f[i]) = j <-> deopt(g[j]) = i)); 