n `nthRoot` x = fst $ until (uncurry(==)) (\(_,x0) -> (x0,((n-1)*x0+x/x0**(n-1))/n)) (x,x/n)
