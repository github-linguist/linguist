function IsPrime( n )
    if n <= 1 or ( n ~= 2 and n % 2 == 0 ) then
        return false
    end

    for i = 3, math.sqrt(n), 2 do
	if n % i == 0 then
  	    return false
	end
    end

    return true
end
