function Test_Function()
    for i = 1, 10000000 do
        local s = math.log( i )
        s = math.sqrt( s )
    end
end

t1 = os.clock()
    Test_Function()
t2 = os.clock()

print( os.difftime( t2, t1 ) )
