pragma.enable("accumulator") # considered experimental

accum [] for x in 1..n { for y in x..n { for z in y..n { if (x**2 + y**2 <=> z**2) { _.with([x,y,z]) } } } }
