function selfie(x)
	y = reverse(digits(x))
	len = length(y)
	sum(y) != len && return false
	for i = 1:len
		y[i] != sum(y .== i-1) && return false
	end
	return true
end
