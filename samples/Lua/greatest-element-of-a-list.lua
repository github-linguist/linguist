-- Table to store values
local values = {}
-- Read in the first number from stdin
local new_val = io.read"*n"
-- Append all numbers passed in
-- until there are no more numbers (io.read'*n' = nil)
while new_val do
  values[#values+1] = new_val
  new_val = io.read"*n"
end

-- Print the max
print(math.max(unpack(values)))
