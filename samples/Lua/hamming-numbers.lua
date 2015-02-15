function hiter()
  hammings = {1}
  prev, vals = {1, 1, 1}
  index = 1
  local function nextv()
    local n, v = 1, hammings[prev[1]]*2
	if hammings[prev[2]]*3 < v then n, v = 2, hammings[prev[2]]*3 end
	if hammings[prev[3]]*5 < v then n, v = 3, hammings[prev[3]]*5 end
	prev[n] = prev[n] + 1
	if hammings[index] == v then return nextv() end
	index = index + 1
	hammings[index] = v
	return v
  end
  return nextv
end

j = hiter()
for i = 1, 20 do
  print(j())
end
n, l = 0, 0
while n < 2^31 do n, l = j(), n end
print(l)
