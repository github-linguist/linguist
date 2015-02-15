function SelectionSort( f )
    for k = 1, #f-1 do
        local idx = k
        for i = k+1, #f do
            if f[i] < f[idx] then
                idx = i
            end
        end
        f[k], f[idx] = f[idx], f[k]
    end
end


f = { 15, -3, 0, -1, 5, 4, 5, 20, -8 }

SelectionSort( f )

for i in next, f do
    print( f[i] )
end
