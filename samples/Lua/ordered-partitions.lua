--- Create a list {1,...,n}.
local function range(n)
  local res = {}
  for i=1,n do
    res[i] = i
  end
  return res
end

--- Return true if the element x is in t.
local function isin(t, x)
  for _,x_t in ipairs(t) do
    if x_t == x then return true end
  end
  return false
end

--- Return the sublist from index u to o (inclusive) from t.
local function slice(t, u, o)
  local res = {}
  for i=u,o do
    res[#res+1] = t[i]
  end
  return res
end

--- Compute the sum of the elements in t.
-- Assume that t is a list of numbers.
local function sum(t)
  local s = 0
  for _,x in ipairs(t) do
    s = s + x
  end
  return s
end

--- Generate all combinations of t of length k (optional, default is #t).
local function combinations(m, r)
  local function combgen(m, n)
    if n == 0 then coroutine.yield({}) end
    for i=1,#m do
      if n == 1 then coroutine.yield({m[i]})
      else
        for m0 in coroutine.wrap(function() combgen(slice(m, i+1, #m), n-1) end) do
          coroutine.yield({m[i], unpack(m0)})
        end
      end
    end
  end
  return coroutine.wrap(function() combgen(m, r) end)
end

--- Generate a list of partitions into fized-size blocks.
local function partitions(...)
  local function helper(s, ...)
    local args = {...}
    if #args == 0 then return {% templatetag openvariable %}{% templatetag closevariable %} end
    local res = {}
    for c in combinations(s, args[1]) do
      local s0 = {}
      for _,x in ipairs(s) do if not isin(c, x) then s0[#s0+1] = x end end
      for _,r in ipairs(helper(s0, unpack(slice(args, 2, #args)))) do
        res[#res+1] = {{unpack(c)}, unpack(r)}
      end
    end
    return res
  end
  return helper(range(sum({...})), ...)
end

-- Print the solution
io.write "["
local parts = partitions(2,0,2)
for i,tuple in ipairs(parts) do
  io.write "("
  for j,set in ipairs(tuple) do
    io.write "{"
    for k,element in ipairs(set) do
      io.write(element)
      if k ~= #set then io.write(", ") end
    end
    io.write "}"
    if j ~= #tuple then io.write(", ") end
  end
  io.write ")"
  if i ~= #parts then io.write(", ") end
end
io.write "]"
io.write "\n"
