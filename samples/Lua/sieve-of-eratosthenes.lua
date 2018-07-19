function erato(n)
  local t = {0, 2}
  for i = 3, n, 2 do t[i], t[i+1] = i, 0 end
  for i = 3, math.sqrt(n) do for j = i*i, n, 2*i do t[j] = 0 end end
  return t
end
