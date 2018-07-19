function fsum(f, a, ...) return a and f(a) + fsum(f, ...) or 0 end
function pymean(t, f, finv) return finv(fsum(f, unpack(t)) / #t) end
nums = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10}

--arithmetic
a = pymean(nums, function(n) return n end, function(n) return n end)
--geometric
g = pymean(nums, math.log, math.exp)
--harmonic
h = pymean(nums, function(n) return 1/n end, function(n) return 1/n end)
print(a, g, h)
assert(a >= g and g >= h)
