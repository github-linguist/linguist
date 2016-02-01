
a = 4
local a = nil

terra foo() return [assert(not a)] end
assert(foo() == true)