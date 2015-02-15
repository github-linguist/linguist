do
  local t = {}
  function f(a, b, ...) if b then return f(a+b, ...) else return a end end
  function average(n)
    if #t == 10 then table.remove(t, 1) end
    t[#t + 1] = n
    return f(unpack(t)) / #t
  end
end
for v=1,30 do print(average(v)) end
