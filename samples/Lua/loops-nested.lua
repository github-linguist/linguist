t = {}
for i = 1, 20 do
  t[i] = {}
  for j = 1, 20 do t[i][j] = math.random(20) end
end
function exitable()
  for i = 1, 20 do
    for j = 1, 20 do
      if t[i][j] == 20 then
        return i, j
      end
    end
  end
end
print(exitable())
