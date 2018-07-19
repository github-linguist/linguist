function nextrow(t)
  local ret = {}
  t[0], t[#t+1] = 0, 0
  for i = 1, #t do ret[i] = t[i-1] + t[i] end
  return ret
end

function triangle(n)
  t = {1}
  for i = 1, n do
    print(unpack(t))
    t = nextrow(t)
  end
end
