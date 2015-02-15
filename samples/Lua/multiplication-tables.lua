io.write( "   |" )
for i = 1, 12 do
    io.write( string.format( "%#5d", i ) )
end
io.write( "\n", string.rep( "-", 12*5+4 ), "\n" )

for i = 1, 12 do
    io.write( string.format( "%#2d |", i ) )

    for j = 1, 12 do
        if j < i then
            io.write( "     " )
        else
            io.write( string.format( "%#5d", i*j ) )
        end
    end
    io.write( "\n" )
end
