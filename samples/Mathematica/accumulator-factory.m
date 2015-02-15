accFactory[initial_] :=
  Module[{total = initial},
    Function[x, total += x]
  ]
x=accFactory[1];
x[5.0];
accFactory[3];
x[2.3]
