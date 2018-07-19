--calculates the nth fibonacci number. Breaks for negative or non-integer n.
function fibs(n)
  return n < 2 and n or fibs(n - 1) + fibs(n - 2)
end

--more pedantic version, returns 0 for non-integer n
function pfibs(n)
  if n ~= math.floor(n) then return 0
  elseif n < 0 then return pfibs(n + 2) - pfibs(n + 1)
  elseif n < 2 then return n
  else return pfibs(n - 1) + pfibs(n - 2)
  end
end

--tail-recursive
function a(n,u,s) if n<2 then return u+s end return a(n-1,u+s,u) end
function trfib(i) return a(i,1,0) end

--table-recursive
fib_n = setmetatable({1, 1}, {__index = function(z,n) return z[n-1] + z[n-2] end})
