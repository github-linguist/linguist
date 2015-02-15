math.randomseed( os.time() )
n = math.random( 1, 10 )

print( "I'm thinking of a number between 1 and 10. Try to guess it: " )

repeat
    x = tonumber( io.read() )

    if x == n then
	print "Well guessed!"
    else
	print "Guess again: "
    end
until x == n
