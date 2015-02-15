filename = "NOTES.TXT"

if #arg == 0 then
    fp = io.open( filename, "r" )
    if fp ~= nil then
        print( fp:read( "*all*" ) )
        fp:close()
    end
else
    fp = io.open( filename, "a+" )
	
    fp:write( os.date( "%x %X\n" ) )
	
    fp:write( "\t" )
    for i = 1, #arg do
	fp:write( arg[i], " " )
    end
    fp:write( "\n" )

    fp:close()
end
