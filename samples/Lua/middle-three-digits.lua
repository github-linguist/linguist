function middle_three(n)
	if n < 0 then
		n = -n
	end
		
	n = tostring(n)
	if #n % 2 == 0 then
		return "Error: the number of digits is even."
	elseif #n < 3 then
		return "Error: the number has less than 3 digits."
	end

	local l = math.floor(#n/2)
	return n:sub(l, l+2)
end

-- test
do
	local t = {123, 12345, 1234567, 987654321,
	10001, -10001, -123, -100, 100, -12345, 1,
	2, -1, -10, 2002, -2002, 0}

	for _,n in pairs(t) do
		print(n, middle_three(n))	
	end
end
