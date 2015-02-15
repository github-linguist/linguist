function Inner( k )
    print( debug.traceback() )
    print "Program continues..."
end

function Middle( x, y )
    Inner( x+y )
end

function Outer( a, b, c )
    Middle( a*b, c )
end

Outer( 2, 3, 5 )
