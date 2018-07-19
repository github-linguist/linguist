function josephus(n, k, m)
    local positions={}
    for i=1,n do
        table.insert(positions, i-1)
    end
    local i,j=1,1
    local s='Execution order: '
    while #positions>m do
        if j==k then
            s=s .. positions[i] .. ', '
            table.remove(positions, i)
            i=i-1
        end
        i=i+1
        j=j+1
        if i>#positions then i=1 end
        if j>k then j=1 end
    end
    print(s:sub(1,#s-2) .. '.')
    local s='Survivors: '
    for _,v in pairs(positions) do s=s .. v .. ', ' end
    print(s:sub(1,#s-2) .. '.')
end
josephus(41,3, 1)
