isharshad(x) = x % sum(digits(x)) == 0
function harshads(n)
	h = Int[]
	i = 1
	while length(h) < n
		isharshad(i) && push!(h,i)
		i+=1
	end
	return h
end
