function gcd(a,b) return a == 0 and b or gcd(b % a, a) end

do
  local function coerce(a, b)
    if type(a) == "number" then return rational(a, 1), b end
    if type(b) == "number" then return a, rational(b, 1) end
    return a, b
  end
  rational = setmetatable({
  __add = function(a, b)
      local a, b = coerce(a, b)
      return rational(a.num * b.den + a.den * b.num, a.den * b.den)
    end,
  __sub = function(a, b)
      local a, b = coerce(a, b)
      return rational(a.num * b.den - a.den * b.num, a.den * b.den)
    end,
  __mul = function(a, b)
      local a, b = coerce(a, b)
      return rational(a.num * b.num, a.den * b.den)
    end,
  __div = function(a, b)
      local a, b = coerce(a, b)
      return rational(a.num * b.den, a.den * b.num)
    end,
  __pow = function(a, b)
      if type(a) == "number" then return a ^ (b.num / b.den) end
      return rational(a.num ^ b, a.den ^ b) --runs into a problem if these aren't integers
    end,
  __concat = function(a, b)
      if getmetatable(a) == rational then return a.num .. "/" .. a.den .. b end
      return a .. b.num .. "/" .. b.den
    end,
  __unm = function(a) return rational(-a.num, -a.den) end}, {
  __call = function(z, a, b) return setmetatable({num = a / gcd(a, b),den = b / gcd(a, b)}, z) end} )
end

print(rational(2, 3) + rational(3, 5) - rational(1, 10) .. "") --> 7/6
print((rational(4, 5) * rational(5, 9)) ^ rational(1, 2) .. "") --> 2/3
print(rational(45, 60) / rational(5, 2) .. "") --> 3/10
print(5 + rational(1, 3) .. "") --> 16/3

function findperfs(n)
  local ret = {}
  for i = 1, n do
    sum = rational(1, i)
    for fac = 2, i^.5 do
      if i % fac == 0 then
        sum = sum + rational(1, fac) + rational(fac, i)
      end
    end
    if sum.den == sum.num then
      ret[#ret + 1] = i
    end
  end
  return table.concat(ret, '\n')
end
print(findperfs(2^19))
