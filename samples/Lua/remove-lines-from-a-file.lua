function remove( filename, starting_line, num_lines )
    local fp = io.open( filename, "r" )
    if fp == nil then return nil end

    content = {}
    i = 1;
    for line in fp:lines() do
        if i < starting_line or i >= starting_line + num_lines then
	    content[#content+1] = line
	end
	i = i + 1
    end

    if i > starting_line and i < starting_line + num_lines then
	print( "Warning: Tried to remove lines after EOF." )
    end

    fp:close()
    fp = io.open( filename, "w+" )

    for i = 1, #content do
	fp:write( string.format( "%s\n", content[i] ) )
    end

    fp:close()
end
