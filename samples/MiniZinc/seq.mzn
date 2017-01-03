par int: n;
array[0..n-1] of var 0..3: x;

constraint x[0] = 0;
constraint x[n-1] = 3;
constraint forall(i in 0..n-2)(x[i] + x[i+1] <= 3);
constraint forall(i in 1..(n div 3))(x[3*i-1] >= 2);

solve maximize sum(x);

output [show(sum(x)) ++ " = " ++ show(x)]