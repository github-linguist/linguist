-- recursive with memoization
catalan = {[0] = 1}
setmetatable(catalan, {
	__index = function(c, n)
			c[n] = c[n-1]*2*(2*n-1)/(n+1)
			return c[n]
		end
	}
)

for i=0,14 do
	print(catalan[i])
end
