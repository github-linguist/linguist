function PrimeDecomposition( n )
    local f = {}

    if IsPrime( n ) then
        f[1] = n
        return f
    end

    local i = 2
    repeat
        while n % i == 0 do
            f[#f+1] = i
            n = n / i
        end

        repeat
            i = i + 1
        until IsPrime( i )
    until n == 1

    return f
end
