function bogosort (list)
    if type (list) ~= 'table' then return list end

    -- Fisher-Yates Knuth shuffle
    local function shuffle ()
        local rand = math.random(1,#list)
        for i=1,#list do
            list[i],list[rand] = list[rand],list[i]
            rand = math.random(1,#list)
        end
    end

    -- Returns true only if list is now sorted
    local function in_order ()
        local last = list[1]
        for i,v in next,list do
            if v < last then return false end
            last = v
        end
        return true
    end

    while not in_order() do shuffle() end

    return list
end
