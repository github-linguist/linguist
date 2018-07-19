function isPerfect(x)
    local sum = 0
    for i = 1, x-1 do
	sum = (x % i) == 0 and sum + i or sum
    end
    return sum == x
end
