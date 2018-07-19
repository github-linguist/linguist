function CountingSort( f )
    local min, max = math.min( unpack(f) ), math.max( unpack(f) )
    local count = {}
    for i = min, max do
        count[i] = 0
    end

    for i = 1, #f do
        count[ f[i] ] = count[ f[i] ] + 1
    end

    local z = 1
    for i = min, max do
        while count[i] > 0 do
            f[z] = i
            z = z + 1
            count[i] = count[i] - 1
        end
    end

end


f = { 15, -3, 0, -1, 5, 4, 5, 20, -8 }

CountingSort( f )

for i in next, f do
    print( f[i] )
end
