-- Open the file named on the command line
local file = assert(io.open(arg[1]))
-- Keep a table counting the instances of each letter
local instances = {}
local function tally(char)
  -- normalize case
  char = string.upper(char)
  -- add to the count of the found character
  occurrences[char] = occurrences[char] + 1
end
-- For each line in the file
for line in file:lines() do
  line:gsub(
    '%a', -- For each letter (%a) on the line,
    tally) --increase the count for that letter
end
-- Print letter counts
for letter, count in pairs(instances) do
  print(letter, count)
end
