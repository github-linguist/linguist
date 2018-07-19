function MonteCarlo ( n_throws )
    math.randomseed( os.time() )

    n_inside = 0
    for i = 1, n_throws do
    	if math.random()^2 + math.random()^2 <= 1.0 then
            n_inside = n_inside + 1
    	end
    end

    return 4 * n_inside / n_throws
end

print( MonteCarlo( 10000 ) )
print( MonteCarlo( 100000 ) )
print( MonteCarlo( 1000000 ) )
print( MonteCarlo( 10000000 ) )
