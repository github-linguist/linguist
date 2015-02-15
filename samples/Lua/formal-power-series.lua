powerseries = setmetatable({
__add = function(z1, z2) return powerseries(function(n) return z1.coeff(n) + z2.coeff(n) end) end,
__sub = function(z1, z2) return powerseries(function(n) return z1.coeff(n) - z2.coeff(n) end) end,
__mul = function(z1, z2) return powerseries(function(n)
  local ret = 0
  for i = 0, n do
    ret = ret + z1.coeff(i) * z2.coeff(n-i)
  end
  return ret
end) end,
__div = function(z1, z2) return powerseries(function(n)
  local ret = z1.coeff(n)
  local function coeffs(a)
    local c = z1.coeff(a)
	for j = 0, a - 1 do c = c - coeffs(j) * z2.coeff(a-j) end
	return c / z2.coeff(0)
  end
  for i = 0, n-1 do
    ret = ret - coeffs(i) * z2.coeff(n-i)
  end
  return ret / z2.coeff(0)
end) end,
__pow = function(z1, p) -- for a series z, z^n returns the nth derivative of z. negative values take integrals.
  if p == 0 then return z1
  elseif p > 0 then return powerseries(function(i) return z1.coeff(i+1)*(i+1) end)^(p-1)
  else return powerseries(function(i) return z1.coeff(i-1)/i end)^(p+1)
  end
end,
__unm = function(z1) return powerseries(function(n) return -z1.coeff(n) end) end,
__index = function(z, n) return z.coeff(n) end,
__call = function(z, n)
  local ret = 0
  for i = 0, 15 do --we do 20 terms, which is simpler than trying to check error bounds
    ret = ret + z[i]*(n^i)
  end
  return ret
end},
{__call = function(z, f) return setmetatable({coeff = f}, z) end})

cosine = powerseries(function(n)
  if(n == 0) then return 1
  else return -((sine^(-1))[n]) --defer to the integral of sine function
  end
end)

sine = powerseries(function(n)
  if(n == 0) then return 0
  else return (cosine^(-1))[n] --defer to the integral of cosine function
  end
end)

print(sine[1], sine[3], sine[5], sine[7], cosine[0], cosine[2], cosine[4], cosine[6])
print(sine(math.pi/3), sine(math.pi/2), cosine(math.pi/3), cosine(math.pi/2))

tangent = sine / cosine
print(tangent(math.pi/3), tangent(math.pi/4), tangent(math.pi/6)) --something like 30000 function calls!
