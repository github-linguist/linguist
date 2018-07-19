suits = {"Clubs", "Diamonds", "Hearts", "Spades"}
faces = {2,3,4,5,6,7,8,9,10,"Jack","Queen","King","Ace"}
--a stack is a set of cards. a stack of length 1 acts as a card; the stack constructor only creates decks.

stack = setmetatable({
--shuffles a stack
__unm = function(z)
  local ret = {}
  for i = #z, 1, -1 do
    ret[#ret + 1] = table.remove(z,math.random(i))
  end
  return setmetatable(ret, stack)
end,
--puts two stacks together
__add = function(z, z2)
  for i = 1, #z2 do
    z[#z+1] = table.remove(z2)
  end
  return z
end,
--removes n cards from a stack and returns a stack of those cards
__sub = function(z, n)
  local ret = {}
  for i = 1, n do
    ret[i] = table.remove(z)
  end
  return setmetatable(ret, stack)
end,
--breaks a stack into n equally sized stacks and returns them all
deal = function(z, n)
  local ret = {}
  for i = 1, #z/n do
    ret[i] = table.remove(z)
  end
  if n > 1 then return setmetatable(ret, stack), stack.deal(z,n-1)
  else return setmetatable(ret, stack)
  end
end,
--returns a and b as strings, concatenated together. Simple enough, right?
__concat = function(a, b)
  if getmetatable(a) == stack then
    return stack.stackstring(a) .. b
  else
    return a .. stack.stackstring(b)
  end
end,
stackstring = function(st, ind)
    ind = ind or 1
	if not st[ind] then return "" end
	return st[ind] and (faces[math.ceil(st[ind]/4)] .. " of " .. suits[st[ind]%4+1] .. "\n" .. stack.stackstring(st, ind+1)) or ""
end}, {
--creates a deck
__call = function(z)
  local ret = {}
  for i = 1, 52 do ret[i] = i end
  return -setmetatable(ret,z)
end})

print(stack() .. "\n")
a, b, c, d = stack.deal(stack(), 4)
print(a .. "\n\n\n")
print(b + c .. "\n\n\n")
print(d - 4 .. "")
print(-b .. "")
