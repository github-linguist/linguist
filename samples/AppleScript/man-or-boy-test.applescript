on a(k, x1, x2, x3, x4, x5)
	script b
		set k to k - 1
		return a(k, b, x1, x2, x3, x4)
	end script
	if k ≤ 0 then
		return (run x4) + (run x5)
	else
		return (run b)
	end if
end a

on int(x)
	script s
		return x
	end script
	return s
end int

a(10, int(1), int(-1), int(-1), int(1), int(0))
