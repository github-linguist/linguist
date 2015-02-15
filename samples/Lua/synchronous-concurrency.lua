function ReadFile()
    local fp = io.open( "input.txt" )
    assert( fp ~= nil )

    for line in fp:lines() do
	coroutine.yield( line )
    end

    fp:close()
end

co = coroutine.create( ReadFile )

while true do
    local status, val = coroutine.resume( co )
    if coroutine.status( co ) == "dead" then break end
    print( val )
end
