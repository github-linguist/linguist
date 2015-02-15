-- Recursive version
function map(f, a, ...) if a then return f(a), map(f, ...) end end
function incr(k) return function(a) return k > a and a or a+1 end end
function combs(m, n)
  if m * n == 0 then return {{}} end
  local ret, old = {}, combs(m-1, n-1)
  for i = 1, n do
    for k, v in ipairs(old) do ret[#ret+1] = {i, map(incr(i), unpack(v))} end
  end
  return ret
end

for k, v in ipairs(combs(3, 5)) do print(unpack(v)) end

-- Iterative version
function icombs(a,b)
    if a==0 then return end
    local taken = {} local slots = {}
    for i=1,a do slots[i]=0 end
    for i=1,b do taken[i]=false end
    local index = 1
    while index > 0 do repeat
        repeat slots[index] = slots[index] + 1
        until slots[index] > b or not taken[slots[index]]
        if slots[index] > b then
            slots[index] = 0
            index = index - 1
            if index > 0 then
                taken[slots[index]] = false
            end
            break
        else
            taken[slots[index]] = true
        end
        if index == a then
            for i=1,a do io.write(slots[i]) io.write(" ") end
            io.write("\n")
            taken[slots[index]] = false
            break
        end
        index = index + 1
    until true end
end

icombs(3, 5)
