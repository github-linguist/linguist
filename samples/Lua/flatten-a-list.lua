function flatten(list)
  if type(list) ~= "table" then return {list} end
  local flat_list = {}
  for _, elem in ipairs(list) do
    for _, val in ipairs(flatten(elem)) do
      flat_list[#flat_list + 1] = val
    end
  end
  return flat_list
end

test_list = {{1}, 2, {{3,4}, 5}, {{{}}}, {{{6}}}, 7, 8, {}}

print(table.concat(flatten(test_list), ","))
