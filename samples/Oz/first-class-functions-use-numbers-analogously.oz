declare

  [X Y Z] = [2.0  4.0  Z=X+Y]
  [XI YI ZI] = [0.5  0.25  1.0/(X+Y)]

  fun {Multiplier A B}
     fun {$ M}
        A * B * M
     end
  end

in

  for
     N in [X  Y  Z]
     I in [XI YI ZI]
  do
     {Show {{Multiplier N I} 0.5}}
  end
