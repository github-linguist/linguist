--returns the powerset of s, out of order.
function powerset(s, start)
  start = start or 1
  if(start > #s) then return {{}} end
  local ret = powerset(s, start + 1)
  for i = 1, #ret do
    ret[#ret + 1] = {s[start], unpack(ret[i])}
  end
  return ret
end

--non-recurse implementation
function powerset(s)
   local t = {{}}
   for i = 1, #s do
      for j = 1, #t do
         t[#t+1] = {s[i],unpack(t[j])}
      end
   end
   return t
end

--alternative, copied from the Python implementation
function powerset2(s)
  local ret = {{}}
  for i = 1, #s do
    local k = #ret
    for j = 1, k do
      ret[k + j] = {s[i], unpack(ret[j])}
    end
  end
  return ret
end
