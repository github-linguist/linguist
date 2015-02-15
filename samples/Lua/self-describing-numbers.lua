function Is_self_describing( n )
    local s = tostring( n )

    local t = {}
    for i = 0, 9 do t[i] = 0 end

    for i = 1, s:len() do
	local idx = tonumber( s:sub(i,i) )
        t[idx] = t[idx] + 1
    end

    for i = 1, s:len() do
        if t[i-1] ~= tonumber( s:sub(i,i) ) then return false end
    end

    return true
end

for i = 1, 999999999 do
    print( Is_self_describing( i ) )
end
