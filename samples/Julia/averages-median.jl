function median2(n)
	s = sort(n)
	len = length(n)
	len%2 == 0 && return (s[ifloor(len/2)+1] + s[ifloor(len/2)])/2
	return s[ifloor(len/2)+1]
end
a = [4.1, 5.6, 7.2, 1.7, 9.3, 4.4, 3.2]
b = [4.1, 7.2, 1.7, 9.3, 4.4, 3.2]
@assert median(a) == median2(a)
@assert median(b) == median2(b)
