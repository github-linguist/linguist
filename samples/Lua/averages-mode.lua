function mode (numlist)
    if type(numlist) ~= 'table' then return numlist end
    local sets = {}
    local mode
    local modeValue = 0
    table.foreach(numlist,function(i,v) if sets[v] then sets[v] = sets[v] + 1 else sets[v] = 1 end end)
    for i,v in next,sets do
        if v > modeValue then
            modeValue = v
            mode = i
        else
            if v == modeValue then
                if type(mode) == 'table' then
                    table.insert(mode,i)
                else
                    mode = {mode,i}
                end
            end
        end
    end
    return mode
end

result = mode({1,3,6,6,6,6,7,7,12,12,17})
print(result)
result = mode({1, 1, 2, 4, 4})
if type(result) == 'table' then
    for i,v in next,result do io.write(v..' ') end
    print ()
end
