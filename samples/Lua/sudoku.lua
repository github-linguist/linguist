--9x9 sudoku solver in lua
--based on a branch and bound solution
--fields are not tried in plain order
--but in a way to detect dead ends earlier
concat=table.concat
insert=table.insert
constraints = { }   --contains a table with 3 constraints for every field
-- a contraint "cons" is a table containing all fields which must not have the same value
-- a field "f" is an integer from 1 to 81
columns = { }       --contains all column-constraints   variable "c"
rows = { }          --contains all row-constraints      variable "r"
blocks = { }        --contains all block-constraints    variable "b"

--initialize all constraints
for f = 1, 81 do
  constraints[f] = { }
end
all_constraints = { } --union of colums, rows and blocks
for i = 1, 9 do
  columns[i] = {
    unknown = 9, --number of fields not yet solved
    unknowns = { } --fields not yet solved
  }
  insert(all_constraints, columns[i])
  rows[i] = {
    unknown = 9, -- see l.15
    unknowns = { } -- see l.16
  }
  insert(all_constraints, rows[i])
  blocks[i] = {
    unknown = 9, --see l.15
    unknowns = { } --see l.16
  }
  insert(all_constraints, blocks[i])
end
constraints_by_unknown = { } --contraints sorted by their number of unknown fields
for i = 0, 9 do
  constraints_by_unknown[i] = {
    count = 0 --how many contraints are in here
  }
end
for r = 1, 9 do
  for c = 1, 9 do
    local f = (r - 1) * 9 + c
    insert(rows[r], f)
    insert(constraints[f], rows[r])
    insert(columns[c], f)
    insert(constraints[f], columns[c])
  end
end
for i = 1, 3 do
  for j = 1, 3 do
    local r = (i - 1) * 3 + j
    for k = 1, 3 do
      for l = 1, 3 do
        local c = (k - 1) * 3 + l
        local f = (r - 1) * 9 + c
        local b = (i - 1) * 3 + k
        insert(blocks[b], f)
        insert(constraints[f], blocks[b])
      end
    end
  end
end
working = { } --save the read values in here
function read() --read the values from stdin
  local f = 1
  local l = io.read("*a")
  for d in l:gmatch("(%d)") do
    local n = tonumber(d)
    if n > 0 then
      working[f] = n
      for _,cons in pairs(constraints[f]) do
        cons.unknown = cons.unknown - 1
      end
    else
      for _,cons in pairs(constraints[f]) do
        cons.unknowns[f] = f
      end
    end
    f = f + 1
  end
  assert((f == 82), "Wrong number of digits")
end
read()
function printer(t) --helper function for printing a 1-81 table
  local pattern = {1,2,3,false,4,5,6,false,7,8,9} --place seperators for better readability
  for _,r in pairs(pattern) do
    if r then
      local function p(c)
        return c and t[(r - 1) * 9 + c] or "|"
      end
      local line={}
      for k,v in pairs(pattern) do
        line[k]=p(v)
      end
      print(concat(line))
    else
      print("---+---+---")
    end
  end
end
order = { } --when to try a field
for _,cons in pairs(all_constraints) do --put all constraints in the corresponding constraints_by_unknown set
  local level = constraints_by_unknown[cons.unknown]
  level[cons] = cons
  level.count = level.count + 1
end
function first(t) --helper function to get a value from a set
  for k, v in pairs(t) do
    if k == v then
      return k
    end
  end
end
function establish_order() -- determine the sequence in which the fields are to be tried
  local solved = constraints_by_unknown[0].count
  while solved < 27 do --there 27 constraints
  --contraints with no unknown fields are considered "solved"
  --keep in mind the actual solving happens in function branch
    local i = 1
    while constraints_by_unknown[i].count == 0 do
      i = i + 1
      -- find a unsolved contraint with the least number of unsolved fields
    end
    local cons = first(constraints_by_unknown[i])
    local f = first(cons.unknowns)
    -- take one of its unknown fields and append it to "order"
    insert(order, f)
    for _,c in pairs(constraints[f]) do
    --each constraint "c" of "f" is moved up one "level"
    --delete "f" from the constraints unknown fields
    --decrease unknown of "c"
      c.unknowns[f] = nil
      local level = constraints_by_unknown[c.unknown]
      level[c] = nil
      level.count = level.count - 1
      c.unknown = c.unknown - 1
      level = constraints_by_unknown[c.unknown]
      level[c] = c
      level.count = level.count + 1
      constraints_by_unknown[c.unknown][c] = c
    end
    solved = constraints_by_unknown[0].count
  end
end
establish_order()
max = #order --how many fields are to be solved
function bound(f,i)
  for _,c in pairs(constraints[f]) do
    for _,x in pairs(c) do
      if i == working[x] then
        return false --i is already used in fs column/row/block
      end
    end
  end
  return true
end
function branch(n)
  local f = order[n] --recursively iterate over fields in order
  if n > max then
    return working --all fields solved without collision
  else
    for i = 1, 9 do --check all values
      if bound(f, i) then --if there is no collision
        working[f] = i
        local res = branch(n + 1) --try next field
        if res then
          return res --all fields solved without collision
        else
          working[f] = nil --this lead to a dead end
        end
      else
        working[f] = nil --reset field because of a collision
      end
    end
    return false --this is a dead end
  end
end
x = branch(1)
if x then
  return printer(x)
end
