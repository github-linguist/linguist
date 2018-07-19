function hailstone(n)
	seq = [n]
	while n>1
		n = n % 2 == 0 ? n >> 1 : 3n + 1
		push!(seq,n)
	end
	return seq
end
