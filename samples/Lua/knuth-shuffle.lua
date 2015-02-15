function table.shuffle(t)
  local n = #t
  while n > 1 do
    local k = math.random(n)
    t[n], t[k] = t[k], t[n]
    n = n - 1
  end

  return t
end
math.randomseed( os.time() )
a = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10}
table.shuffle(a)
for i,v in ipairs(a) do print(i,v) end
