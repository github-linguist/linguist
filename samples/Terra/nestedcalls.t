
terra foo3(a : &int) : &int
  return a
end

terra bar2(a : int)
  return @(foo3(&a))
end

local test = require("test")
test.eq(bar2(42),42)