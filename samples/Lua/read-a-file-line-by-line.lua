filename = "input.txt"
fp = io.open( filename, "r" )

for line in fp:lines() do
    print( line )
end

fp:close()
