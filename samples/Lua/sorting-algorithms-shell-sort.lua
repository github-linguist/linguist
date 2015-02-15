function shellsort( a )
    local inc = math.ceil( #a / 2 )
    while inc > 0 do
        for i = inc, #a do
            local tmp = a[i]
            local j = i
            while j > inc and a[j-inc] > tmp do
                a[j] = a[j-inc]
                j = j - inc
            end
            a[j] = tmp
        end
        inc = math.floor( 0.5 + inc / 2.2 )
    end

    return a
end

a = { -12, 3, 0, 4, 7, 4, 8, -5, 9 }
a = shellsort( a )

for _, i in pairs(a) do
    print(i)
end
