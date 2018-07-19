function digits(n)
  if n > 0 then return n % 10, digits(math.floor(n/10)) end
end
function sumsq(a, ...)
  return a and a ^ 2 + sumsq(...) or 0
end
local happy = setmetatable({true, false, false, false}, {
      __index = function(self, n)
         self[n] = self[sumsq(digits(n))]
         return self[n]
      end } )
i, j = 0, 1
repeat
   i, j = happy[j] and (print(j) or i+1) or i, j + 1
until i == 8
