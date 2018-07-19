function sum(var, a, b, str)
  local ret = 0
  for i = a, b do
    ret = ret + setfenv(loadstring("return "..str), {[var] = i})()
  end
  return ret
end
print(sum("i", 1, 100, "1/i"))
