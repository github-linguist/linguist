function filter(t, func)
  local ret = {}
  for i, v in ipairs(t) do ret[#ret+1] = func(v) and v or nil end
  return ret
end

function even(a) return a % 2 == 0 end

print(unpack(filter({1,2,3,4,5,6,7,8,9,10},even)))
