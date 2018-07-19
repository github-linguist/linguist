declare
  fun {NthRoot NInt A}
     N = {Int.toFloat NInt}

     fun {Next X}
        ( (N-1.0)*X + A / {Pow X N-1.0} ) / N
     end
  in
     {Until Value.'==' Next A/N}
  end

  fun {Until P F X}
     case {F X}
     of NX andthen {P NX X} then X
     [] NX then {Until P F NX}
     end
  end
in
  {Show {NthRoot 2 2.0}}
