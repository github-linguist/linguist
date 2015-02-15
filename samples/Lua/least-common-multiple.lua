function gcd( m, n )
    while n ~= 0 do
        local q = m
        m = n
        n = q % n
    end
    return m
end

function lcm( m, n )
    return ( m ~= 0 and n ~= 0 ) and m * n / gcd( m, n ) or 0
end

print( lcm(12,18) )
