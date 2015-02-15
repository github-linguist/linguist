function map_range( a1, a2, b1, b2, s )
    return b1 + (s-a1)*(b2-b1)/(a2-a1)
end

for i = 0, 10 do
    print( string.format( "f(%d) = %f", i, map_range( 0, 10, -1, 0, i ) ) )
end
