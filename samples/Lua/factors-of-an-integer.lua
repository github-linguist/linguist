function Factors( n )
    local f = {}

    for i = 1, n/2 do
        if n % i == 0 then
            f[#f+1] = i
        end
    end
    f[#f+1] = n

    return f
end
