--defines addition, subtraction, negation, multiplication, division, conjugation, norms, and a conversion to strgs.
complex = setmetatable({
__add = function(u, v) return complex(u.real + v.real, u.imag + v.imag) end,
__sub = function(u, v) return complex(u.real - v.real, u.imag - v.imag) end,
__mul = function(u, v) return complex(u.real * v.real - u.imag * v.imag, u.real * v.imag + u.imag * v.real) end,
__div = function(u, v) return u * complex(v.real / v.norm, -v.imag / v.norm) end,
__unm = function(u) return complex(-u.real, -u.imag) end,
__concat = function(u, v)
    if type(u) == "table" then return u.real .. " + " .. u.imag .. "i" .. v
	elseif type(u) == "string" or type(u) == "number" then return u .. v.real .. " + " .. v.imag .. "i"
	end end,
__index = function(u, index)
  local operations = {
    norm = function(u) return u.real ^ 2 + u.imag ^ 2 end,
    conj = function(u) return complex(u.real, -u.imag) end,
  }
  return operations[index] and operations[index](u)
end,
__newindex = function() error() end
}, {
__call = function(z, realpart, imagpart) return setmetatable({real = realpart, imag = imagpart}, complex) end
} )
n = io.read() + 0
val = complex(math.cos(2*math.pi / n), math.sin(2*math.pi / n))
root = complex(1, 0)
for i = 1, n do
  root = root * val
  print(root .. "")
end
