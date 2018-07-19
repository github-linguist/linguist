function dotprod(a, b)
  local ret = 0
  for i = 1, #a do
    ret = ret + a[i] * b[i]
  end
  return ret
end

print(dotprod({1, 3, -5}, {4, -2, 1}))
