function factorize( n )
    if n == 1 then return {1} end

    local k = 2
    res = {}
    while n > 1 do
	while n % k == 0 do
	    res[#res+1] = k
 	    n = n / k
	end
 	k = k + 1
    end
    return res
end

for i = 1, 22 do
    io.write( i, ":  " )
    fac = factorize( i )
    io.write( fac[1] )
    for j = 2, #fac do
	io.write( " * ", fac[j] )
    end
    print ""
end
